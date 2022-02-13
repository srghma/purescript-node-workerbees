module Node.WorkerBees
  ( WorkerContext
  , WorkerThread
  , WorkerOptions
  , Worker
  , ThreadId(..)
  , make
  , makeAsMain
  , unsafeWorkerFromPath
  , unsafeWorkerFromPathAndExport
  , lift
  , liftReader
  , liftEffect
  , liftReaderT
  , spawn
  , post
  , terminate
  , class Sendable
  , class SendableRowList
  , SendWrapper
  , wrap
  , unsafeWrap
  , unwrap
  ) where

import Prelude

import Control.Monad.Reader (Reader, ReaderT, runReader, runReaderT)
import Data.Argonaut.Core (Json)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn2, EffectFn4, EffectFn5, runEffectFn2, runEffectFn4, runEffectFn5)
import Foreign.Object (Object)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as Row
import Prim.TypeError (class Fail, Beside, Quote, Text)

newtype ThreadId = ThreadId Int

derive instance eqThreadId :: Eq ThreadId
derive instance ordThreadId :: Ord ThreadId

type WorkerContext workerData receiveInput replyOutput =
  { exit :: Effect Unit
  , receive :: (receiveInput -> Effect Unit) -> Effect Unit
  , reply :: replyOutput -> Effect Unit
  , threadId :: ThreadId
  , workerData :: workerData
  }

type WorkerOptions workerData replyOutput =
  { onError :: Error -> Effect Unit
  , onExit :: Int -> Effect Unit
  , onMessage :: replyOutput -> Effect Unit
  , workerData :: workerData
  }

type WorkerConstructor workerData receiveInput replyOutput = WorkerContext workerData receiveInput replyOutput -> Effect Unit

-- foreign import data Worker :: Type -> Type -> Type -> Type

type Worker = 
  { resolve :: EffectFn2 ErrorOrUndefined { filePath :: String, export :: String } Unit
  , spawn :: Effect Unit
  } 
-- workerData, receiveInput, replyOutput

foreign import data WorkerThread :: Type -> Type 
-- `receive` input

foreign import makeImpl 
  :: forall workerData receiveInput replyOutput
  . WorkerConstructor workerData receiveInput replyOutput 
  -> Worker workerData receiveInput replyOutput

foreign import unsafeMakeImpl 
  :: forall workerData receiveInput replyOutput
  . { filePath :: String, export :: String }
  -> Worker workerData receiveInput replyOutput

foreign import mainImpl :: forall workerData receiveInput replyOutput. WorkerConstructor workerData receiveInput replyOutput -> Effect Unit

foreign import spawnImpl 
  :: forall workerData receiveInput replyOutput
  . EffectFn5 (forall x y. x -> Either x y) 
  (forall x y. y -> Either x y) 
  (Worker workerData receiveInput replyOutput) 
  (WorkerOptions workerData replyOutput) 
  (Either Error (WorkerThread receiveInput) -> Effect Unit) 
  Unit

foreign import postImpl :: forall receiveInput. EffectFn2 receiveInput (WorkerThread receiveInput) Unit

foreign import terminateImpl
  :: forall receiveInput
  . EffectFn4 (forall x y. x -> Either x y) 
  (forall x y. y -> Either x y) 
  (WorkerThread receiveInput) 
  (Either Error Unit -> Effect Unit) 
  Unit

foreign import threadId :: forall receiveInput. WorkerThread receiveInput -> ThreadId

-- | Builds a new Worker. Treat this like it's special top-level declaration
-- | syntax. Workers can only be declared at the top-level with `make`, and they
-- | _must_ be exported. Failing to meet these criteria will result in a runtime
-- | exception.
make :: forall workerData receiveInput replyOutput. Sendable replyOutput => WorkerConstructor workerData receiveInput replyOutput -> Worker workerData receiveInput replyOutput
make = makeImpl

makeAsMain :: forall workerData receiveInput replyOutput. Sendable replyOutput => WorkerConstructor workerData receiveInput replyOutput -> Effect Unit
makeAsMain = mainImpl

unsafeWorkerFromPath :: forall workerData receiveInput replyOutput. Sendable replyOutput => String -> Worker workerData receiveInput replyOutput
unsafeWorkerFromPath = unsafeMakeImpl <<< { filePath: _, export: "" }

unsafeWorkerFromPathAndExport :: forall workerData receiveInput replyOutput. Sendable replyOutput => { filePath :: String, export :: String } -> Worker workerData receiveInput replyOutput
unsafeWorkerFromPathAndExport = unsafeMakeImpl

-- | Instantiates a new worker thread. If this worker subscribes to input, it
-- | will need to be cleaned up with `terminate`, otherwise it will hold your
-- | process open.
spawn :: forall workerData receiveInput replyOutput. Sendable workerData => Worker workerData receiveInput replyOutput -> WorkerOptions workerData replyOutput -> (Either Error (WorkerThread receiveInput) -> Effect Unit) -> Effect Unit
spawn = runEffectFn5 spawnImpl Left Right

-- | Sends some input to a worker thread to process.
post :: forall receiveInput. Sendable receiveInput => receiveInput -> WorkerThread receiveInput -> Effect Unit
post = runEffectFn2 postImpl

-- | Terminates the worker thread.
terminate :: forall receiveInput. WorkerThread receiveInput -> (Either Error Unit -> Effect Unit) -> Effect Unit
terminate = runEffectFn4 terminateImpl Left Right

-- | Only Sendable things can be sent back and forth between a worker thread and
-- | its parent. These include things that are represented by JavaScript primitives.
-- | Arbitrary PureScript values cannot be sent, but variants, records and newtypes
-- | of these things can. If you have a newtype of some Sendable, you must wrap it.
class Sendable (workerData :: Type)

instance sendableInt :: Sendable Int
else instance sendableNumber :: Sendable Number
else instance sendableString :: Sendable String
else instance sendableBoolean :: Sendable Boolean
else instance sendableArray :: Sendable a => Sendable (Array a)
else instance sendableObject :: Sendable a => Sendable (Object a)
else instance sendableRecord :: (RowToList r rl, SendableRowList rl) => Sendable (Record r)
else instance sendableVariant :: (RowToList r rl, SendableRowList rl) => Sendable (Variant r)
else instance sendableSendWrap :: Sendable (SendWrapper a)
else instance sendableJson :: Sendable Json
else instance sendableUnit :: Sendable Unit
else instance sendableVoid :: Sendable Void
else instance sendableArrayBuffer :: Sendable ArrayBuffer
else instance sendableFail :: Fail (Beside (Quote a) (Text " is not known to be Sendable")) => Sendable a

class SendableRowList (rl :: RowList Type)

instance sendableRowListNil :: SendableRowList Row.Nil
instance sendableRowListCons :: (Sendable a, SendableRowList rest) => SendableRowList (Row.Cons sym a rest)

-- | For newtypes that are otherwise Sendable.
newtype SendWrapper a = SendWrapper a

wrap :: forall a b. Newtype a b => Sendable b => a -> SendWrapper a
wrap = SendWrapper

unwrap :: forall a. SendWrapper a -> a
unwrap (SendWrapper a) = a

-- | Use with care. If you send something that isn't actually Sendable, it
-- | will raise an exception.
unsafeWrap :: forall a. a -> SendWrapper a
unsafeWrap = SendWrapper

lift :: forall workerData receiveInput replyOutput. (receiveInput -> replyOutput) -> WorkerConstructor workerData receiveInput replyOutput
-- lift :: forall workerData receiveInput replyOutput. (receiveInput -> replyOutput) -> WorkerContext workerData receiveInput replyOutput -> Effect Unit
lift k { receive, reply } = receive (reply <<< k)

liftReader :: forall workerData receiveInput replyOutput. (receiveInput -> Reader workerData replyOutput) -> WorkerConstructor workerData receiveInput replyOutput
liftReader k { receive, reply, workerData } = receive (reply <<< flip runReader workerData <<< k)

liftEffect :: forall workerData receiveInput replyOutput. (receiveInput -> Effect replyOutput) -> WorkerConstructor workerData receiveInput replyOutput
liftEffect k { receive, reply } = receive (reply <=< k)

liftReaderT :: forall workerData receiveInput replyOutput. (receiveInput -> ReaderT workerData Effect replyOutput) -> WorkerConstructor workerData receiveInput replyOutput
liftReaderT k { receive, reply, workerData } = receive (reply <=< flip runReaderT workerData <<< k)
