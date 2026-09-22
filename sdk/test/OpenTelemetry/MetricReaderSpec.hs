{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.MetricReaderSpec (spec) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.IORef
import qualified Data.Vector as V
import OpenTelemetry.Configuration.Create (OTelSignals (..), createFromConfig)
import qualified OpenTelemetry.Configuration.Types as Config
import OpenTelemetry.Exporter.Metric (
  MetricExport (..),
  MetricExporter (..),
  NumberValue (..),
  ResourceMetricsExport (..),
  ScopeMetricsExport (..),
  SumDataPoint (..),
 )
import OpenTelemetry.Internal.Common.Types (ExportResult (..), FlushResult (..), ShutdownResult (..))
import OpenTelemetry.MeterProvider (
  SdkMeterProviderOptions (..),
  createMeterProvider,
  defaultSdkMeterProviderOptions,
 )
import qualified OpenTelemetry.Metric as Metric
import OpenTelemetry.MetricReader
import OpenTelemetry.Resource (emptyMaterializedResources)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec


spec :: Spec
spec = describe "MetricReader" $ do
  accountingSpec
  describe "defaultPeriodicMetricReaderOptions" $ do
    it "has 60s interval" $ do
      periodicIntervalMicros defaultPeriodicMetricReaderOptions `shouldBe` 60_000_000

  describe "exportMetricsOnce" $ do
    it "collects and exports a single batch" $ do
      exportCountRef <- newIORef (0 :: Int)
      let exporter =
            MetricExporter
              { metricExporterExport = \_ -> do
                  modifyIORef' exportCountRef (+ 1)
                  pure Success
              , metricExporterShutdown = pure ShutdownSuccess
              , metricExporterForceFlush = pure FlushSuccess
              }
      (_mp, env) <- createMeterProvider emptyMaterializedResources defaultSdkMeterProviderOptions {metricExporter = Just exporter}
      result <- exportMetricsOnce env exporter
      case result of
        Success -> pure ()
        _ -> expectationFailure "expected Success"
      count <- readIORef exportCountRef
      count `shouldBe` 1

  describe "forkPeriodicMetricReader" $ do
    it "can be started and stopped" $ do
      exportCountRef <- newIORef (0 :: Int)
      let exporter =
            MetricExporter
              { metricExporterExport = \_ -> do
                  modifyIORef' exportCountRef (+ 1)
                  pure Success
              , metricExporterShutdown = pure ShutdownSuccess
              , metricExporterForceFlush = pure FlushSuccess
              }
          opts =
            PeriodicMetricReaderOptions
              { periodicIntervalMicros = 100_000
              }
      (_mp, env) <- createMeterProvider emptyMaterializedResources defaultSdkMeterProviderOptions {metricExporter = Just exporter}
      handle <- forkPeriodicMetricReader env exporter opts
      stopPeriodicMetricReader handle
      count <- readIORef exportCountRef
      count `shouldSatisfy` (>= 1)


-- These tests change process-global configuration and restore it afterwards.
withMetricEnvironment :: Maybe String -> IO a -> IO a
withMetricEnvironment exporter action =
  bracket Metric.getGlobalMeterProvider Metric.setGlobalMeterProvider $ \_ ->
    withEnvValue "OTEL_SDK_DISABLED" (Just "false") $
      withEnvValue "OTEL_METRICS_EXPORTER" exporter action


withEnvValue :: String -> Maybe String -> IO a -> IO a
withEnvValue name value action = bracket (lookupEnv name) restore $ \_ -> restore value >> action
  where
    restore = maybe (unsetEnv name) (setEnv name)


assertNoAccounting :: Metric.MeterProvider -> Expectation
assertNoAccounting provider = do
  meter <- Metric.getMeter provider "disabled-test"
  let adv = Metric.defaultAdvisoryParameters
      unused = error "disabled metrics must not evaluate measurements or attributes"
  ci <- Metric.meterCreateCounterInt64 meter "counter.int" Nothing Nothing adv
  cd <- Metric.meterCreateCounterDouble meter "counter.double" Nothing Nothing adv
  ui <- Metric.meterCreateUpDownCounterInt64 meter "updown.int" Nothing Nothing adv
  ud <- Metric.meterCreateUpDownCounterDouble meter "updown.double" Nothing Nothing adv
  h <- Metric.meterCreateHistogram meter "histogram" Nothing Nothing adv
  gi <- Metric.meterCreateGaugeInt64 meter "gauge.int" Nothing Nothing adv
  gd <- Metric.meterCreateGaugeDouble meter "gauge.double" Nothing Nothing adv
  sequence
    [ Metric.counterEnabled ci
    , Metric.counterEnabled cd
    , Metric.upDownCounterEnabled ui
    , Metric.upDownCounterEnabled ud
    , Metric.histogramEnabled h
    , Metric.gaugeEnabled gi
    , Metric.gaugeEnabled gd
    ]
    `shouldReturn` replicate 7 False
  Metric.counterAdd ci unused unused
  Metric.counterAdd cd unused unused
  Metric.upDownCounterAdd ui unused unused
  Metric.upDownCounterAdd ud unused unused
  Metric.histogramRecord h unused unused
  Metric.gaugeRecord gi unused unused
  Metric.gaugeRecord gd unused unused
  calls <- newIORef (0 :: Int)
  let callback _ = modifyIORef' calls (+ 1)
  oci <- Metric.meterCreateObservableCounterInt64 meter "oc.int" Nothing Nothing adv [callback]
  ocd <- Metric.meterCreateObservableCounterDouble meter "oc.double" Nothing Nothing adv [callback]
  oui <- Metric.meterCreateObservableUpDownCounterInt64 meter "ou.int" Nothing Nothing adv [callback]
  oud <- Metric.meterCreateObservableUpDownCounterDouble meter "ou.double" Nothing Nothing adv [callback]
  ogi <- Metric.meterCreateObservableGaugeInt64 meter "og.int" Nothing Nothing adv [callback]
  ogd <- Metric.meterCreateObservableGaugeDouble meter "og.double" Nothing Nothing adv [callback]
  sequence
    [ Metric.observableCounterEnabled oci
    , Metric.observableCounterEnabled ocd
    , Metric.observableUpDownCounterEnabled oui
    , Metric.observableUpDownCounterEnabled oud
    , Metric.observableGaugeEnabled ogi
    , Metric.observableGaugeEnabled ogd
    ]
    `shouldReturn` replicate 6 False
  Metric.forceFlushMeterProvider provider Nothing `shouldReturn` FlushSuccess
  Metric.shutdownMeterProvider provider Nothing `shouldReturn` ShutdownSuccess
  Metric.shutdownMeterProvider provider Nothing `shouldReturn` ShutdownSuccess
  readIORef calls `shouldReturn` 0


accountingSpec :: Spec
accountingSpec = describe "metric accounting configuration" $ do
  it "disables environment-configured metrics for none" $
    withMetricEnvironment (Just "none") $
      Metric.withMeterProvider $ \provider -> do
        assertNoAccounting provider
        Metric.getGlobalMeterProvider >>= assertNoAccounting

  it "keeps explicit providers usable for manual collection when the environment selects none" $
    withMetricEnvironment (Just "none") $ do
      (provider, env) <- createMeterProvider emptyMaterializedResources defaultSdkMeterProviderOptions
      meter <- Metric.getMeter provider "manual-reader"
      c <- Metric.meterCreateCounterInt64 meter "requests" Nothing Nothing Metric.defaultAdvisoryParameters
      Metric.counterEnabled c `shouldReturn` True
      Metric.counterAdd c 7 Metric.emptyAttributes
      seen <- newIORef []
      let exporter =
            MetricExporter
              { metricExporterExport = \batches -> writeIORef seen (V.toList batches) >> pure Success
              , metricExporterForceFlush = pure FlushSuccess
              , metricExporterShutdown = pure ShutdownSuccess
              }
      _ <- exportMetricsOnce env exporter
      batches <- readIORef seen
      let values =
            [ sumDataPointValue point
            | batch <- batches
            , scope <- V.toList (resourceMetricsScopes batch)
            , MetricExportSum {mesSumPoints = points} <- V.toList (scopeMetricsExports scope)
            , point <- V.toList points
            ]
      values `shouldBe` [IntNumber 7]
      Metric.shutdownMeterProvider provider Nothing `shouldReturn` ShutdownSuccess

  it "preserves the default OTLP provider when the variable is unset" $
    withMetricEnvironment Nothing $
      Metric.withMeterProvider $ \provider -> do
        meter <- Metric.getMeter provider "default-exporter"
        c <- Metric.meterCreateCounterInt64 meter "requests" Nothing Nothing Metric.defaultAdvisoryParameters
        Metric.counterEnabled c `shouldReturn` True

  it "preserves Prometheus accounting without a push exporter" $
    withMetricEnvironment (Just "prometheus") $
      Metric.withMeterProvider $ \provider -> do
        meter <- Metric.getMeter provider "pull-reader"
        c <- Metric.meterCreateCounterInt64 meter "requests" Nothing Nothing Metric.defaultAdvisoryParameters
        Metric.counterEnabled c `shouldReturn` True

  let reader exporter = Config.MetricReaderPeriodic (Config.PeriodicMetricReaderConfig Nothing Nothing exporter)
      configured readers = Config.emptyConfiguration {Config.configMeterProvider = Just (Config.MeterProviderConfig readers)}
      console = Config.PushMetricExporterConsole Config.ConsoleExporterConfig
  forM_
    [ ("absent meter provider", Config.emptyConfiguration)
    , ("absent readers", configured Nothing)
    , ("empty readers", configured (Just []))
    , ("reader without an exporter", configured (Just [reader Config.PushMetricExporterNone]))
    , ("disabled SDK with an exporter", (configured (Just [reader console])) {Config.configDisabled = Just True})
    ]
    $ \(label, cfg) ->
      it ("disables declarative metrics with " ++ label) $
        bracket (createFromConfig cfg) otelShutdown $
          \signals -> assertNoAccounting (otelMeterProvider signals)

  it "preserves an explicitly configured exporter even when the environment selects none" $
    withMetricEnvironment (Just "none") $
      bracket (createFromConfig (configured (Just [reader console]))) otelShutdown $ \signals -> do
        meter <- Metric.getMeter (otelMeterProvider signals) "configured-exporter"
        c <- Metric.meterCreateCounterInt64 meter "requests" Nothing Nothing Metric.defaultAdvisoryParameters
        Metric.counterEnabled c `shouldReturn` True
