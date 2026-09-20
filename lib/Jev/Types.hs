-- | Configuration, typed answers, and credential-safe failures.
-- Fields support record-dot syntax and pattern matching; there are no ordinary
-- record selector functions. Public answer constructors do not enforce invariants.
module Jev.Types
  ( Provider (..),
    Config (..),
    defaultConfig,
    RequestOptions (..),
    defaultRequestOptions,
    Option (..),
    JsonOption (..),
    NoulCriteria (..),
    Choice (..),
    Score (..),
    Noul (..),
    Response (..),
    Usage (..),
    ResponseMetadata (..),
    TransportFailure (..),
    JevError (..),
  )
where

import Data.Aeson (Value)
import Data.ByteString.Lazy qualified as LBS
import Data.IntMap.Strict (IntMap)
import Data.Text (Text)
import Network.HTTP.Types.Header (ResponseHeaders)

-- | The gateway used for validation and the default endpoint.
data Provider = TypeSafe | OpenRouter deriving (Eq, Show)

-- | Reusable client settings. Deliberately has no 'Show' instance to protect keys.
data Config = Config
  { -- | Gateway; distinct from the provider reported in a response.
    provider :: Provider,
    -- | Explicit bearer token. No environment lookup is performed.
    apiKey :: Text,
    -- | Model name or alias; pin a version for reproducible thresholds.
    model :: Text,
    -- | Complete URL override, including path. 'Nothing' selects the gateway default.
    endpoint :: Maybe Text,
    -- | Positive deadline for the entire HTTP operation, including the body, in microseconds. Excludes validation and response decoding.
    timeoutMicros :: Int
  }

-- | Defaults to a 30-second HTTP deadline and the gateway's documented endpoint.
-- TypeSafe uses @jev-latest@; OpenRouter uses @typesafe/jev-1.13@.
defaultConfig :: Provider -> Text -> Config
defaultConfig provider apiKey = Config provider apiKey model Nothing 30000000
  where
    model = case provider of
      TypeSafe -> "jev-latest"
      OpenRouter -> "typesafe/jev-1.13"

-- | Optional OpenRouter request settings. Nonempty settings are rejected for
-- TypeSafe rather than silently ignored. Nested routing and trace schemas are
-- validated by OpenRouter. Avoid putting secrets in observability metadata.
data RequestOptions = RequestOptions
  { -- | OpenRouter @provider@ object.
    providerRouting :: Maybe Value,
    -- | Observability session identifier, at most 256 characters.
    sessionId :: Maybe Text,
    -- | OpenRouter trace metadata object.
    trace :: Maybe Value,
    -- | End-user identifier, at most 256 characters.
    user :: Maybe Text
  }
  deriving (Eq, Show)

-- | No routing or observability overrides; works with both gateways.
defaultRequestOptions :: RequestOptions
defaultRequestOptions = RequestOptions Nothing Nothing Nothing Nothing

type role Option representational

-- | @Option value label description@: domain value, unique wire label, and
-- optional description. Values need no typeclass instances; labels must be unique.
data Option a = Option a Text (Maybe Text) deriving (Eq, Show, Functor)

type role JsonOption representational

-- | Structured version of @Option@. Descriptions accept strings, objects, arrays,
-- or null. Both 'Nothing' and @Just Null@ encode an undescribed option.
data JsonOption a = JsonOption a Text (Maybe Value) deriving (Eq, Show, Functor)

type role NoulCriteria representational

-- | Descriptions of the yes and no outcomes, in that order.
data NoulCriteria a = NoulCriteria
  { -- | Meaning of a yes answer.
    true :: a,
    -- | Meaning of a no answer.
    false :: a
  }
  deriving (Eq, Show, Functor)

type role Choice representational

-- | A selected domain value and the distribution over all supplied options.
-- 'fmap' transforms the selected value and every distribution entry.
data Choice a = Choice
  { -- | An option with maximal probability, allowing rounding tolerance.
    choice :: a,
    -- | Provider confidence in [0,1]; not the selected probability.
    confidence :: Double,
    -- | Distribution in the original option order, summing approximately to one.
    probabilities :: [(a, Double)]
  }
  deriving (Eq, Show, Functor)

-- | An expected value on the zero-based rubric scale, not normalized to [0,1].
data Score = Score
  { -- | Probability-weighted rubric index, possibly fractional.
    score :: Double,
    -- | Provider confidence in [0,1].
    confidence :: Double,
    -- | Sparse distribution; missing levels are not inserted. Sum is approximately one.
    probabilities :: IntMap Double,
    -- | Returned descriptions; includes every supplied probability index. Sparse legends are preserved.
    legend :: IntMap Value
  }
  deriving (Eq, Show)

-- | Probability of yes, in [0,1]. Choose application-specific thresholds.
newtype Noul = Noul {probability :: Double} deriving (Eq, Show)

-- | Usage fields are absent when the gateway does not report them.
data Usage = Usage
  { -- | Input token count.
    inputTokens :: Maybe Int,
    -- | Output token count.
    outputTokens :: Maybe Int,
    -- | OpenRouter-reported cost in USD, when present.
    cost :: Maybe Double
  }
  deriving (Eq, Show)

type role Response representational

-- | Typed answers with provider metadata. 'fmap' changes only the answers.
data Response a = Response
  { -- | Result of the composed question.
    answers :: a,
    -- | Resolved model reported by the gateway.
    model :: Text,
    -- | Reported usage; individual fields are optional.
    usage :: Usage,
    -- | Body @id@, falling back to @x-typesafe-request-id@.
    requestId :: Maybe Text,
    -- | Provider name reported by the gateway, if supplied.
    provider :: Maybe Text
  }
  deriving (Eq, Show, Functor)

-- | HTTP context retained even when a response cannot be decoded.
-- Headers and error bodies are server-controlled and may contain sensitive data.
data ResponseMetadata = ResponseMetadata
  { -- | HTTP status code.
    statusCode :: Int,
    -- | Unmodified response headers, including any @Retry-After@.
    headers :: ResponseHeaders,
    -- | Body @id@, falling back to @x-typesafe-request-id@.
    requestId :: Maybe Text
  }
  deriving (Eq, Show)

-- | Stable categories without exception text, URLs, request bodies, or credentials.
-- DNS and TLS failures may be reported as connection or internal failures by
-- the underlying manager. These categories do not imply a request is safe to retry.
data TransportFailure
  = -- | Total HTTP deadline expired, including body consumption.
    DeadlineExceeded
  | -- | The manager's response timeout expired.
    ResponseTimedOut
  | -- | Connection establishment timed out.
    ConnectionTimedOut
  | -- | The endpoint could not be parsed.
    InvalidEndpoint
  | -- | A connection could not be established or used.
    ConnectionFailed
  | -- | The connection closed unexpectedly.
    ConnectionClosed
  | -- | Malformed, truncated, or otherwise invalid HTTP response.
    InvalidResponse
  | -- | The supplied manager cannot make TLS requests.
    TlsNotSupported
  | -- | An internal manager exception, potentially a TLS failure.
    InternalTransportFailure
  | -- | Another HTTP transport failure.
    OtherTransportFailure
  deriving (Eq, Show)

-- | Expected failures are returned in 'Either'; asynchronous cancellation propagates.
-- Custom manager hooks and user-supplied pure functions can still throw exceptions.
data JevError
  = -- | Invalid local input; no HTTP request was made.
    ValidationError Text
  | -- | Categorized, credential-safe transport failure.
    TransportError TransportFailure
  | -- | Non-2xx response with metadata and original body.
    HttpError ResponseMetadata LBS.ByteString
  | -- | Pure fixture decoding failed; no HTTP metadata is available.
    DecodeError Text
  | -- | A successful HTTP response contained an invalid answer.
    ResponseDecodeError ResponseMetadata Text
  deriving (Eq, Show)
