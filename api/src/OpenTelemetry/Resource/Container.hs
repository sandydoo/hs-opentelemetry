module OpenTelemetry.Resource.Container where
import Data.Text (Text)

data Container = Container
  { containerName :: Maybe Text
  -- ^ Container name.
  , containerId :: Maybe Text
  -- ^ Container ID. Usually a UUID, as for example used to identify Docker containers. The UUID might be abbreviated.
  , containerRuntime :: Maybe Text
  -- ^ The container runtime managing this container.
  , containerImageName :: Maybe Text
  -- ^ Name of the image the container was built on.
  , containerImageTag :: Maybe Text
  -- ^ Container image tag.
  }