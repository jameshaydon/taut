module Lib where

import Control.Concurrent
import Control.Lens hiding (argument)
import qualified Data.Text as Text
import Data.Text (Text)
import Network.Wreq hiding (header)
import Options.Applicative
import System.Process (callCommand)

type Cycle = [Step]

data DndStatus = Focus | Slack
  deriving stock (Show)

type LastStep = DndStatus

type Step = (DndStatus, Int)

data Config = Config
  { token :: Text,
    chan :: Text,
    startWith :: DndStatus,
    duration :: Int,
    notifier :: Maybe Text
  }
  deriving stock (Show)

statReader :: ReadM DndStatus
statReader = eitherReader $ \case
  "focus" -> pure Focus
  "slack" -> pure Slack
  _ -> Left "Needs to be one of 'focus' or 'slack'."

config :: Parser Config
config =
  Config
    <$> strOption
      ( long "token"
          <> short 't'
          <> help "Slack legacy token"
          <> metavar "TOKEN"
      )
    <*> strOption
      ( long "channel"
          <> short 'c'
          <> help "The channel to post the command to. This should be slackbot or your self-DM."
          <> metavar "CHANNEL"
      )
    <*> option
      statReader
      ( long "start"
          <> short 's'
          <> help "The starting status, one of 'focus' or 'slack'"
          <> showDefault
          <> value Slack
          <> metavar "STATUS"
      )
    <*> option
      auto
      ( long "duration"
          <> short 'd'
          <> help "Duration of the first phase"
          <> showDefault
          <> value 10
          <> metavar "INT"
      )
    <*> optional
      ( strOption
          ( long "notifier"
              <> short 'n'
              <> help "A command to send notifications"
              <> metavar "CMD"
          )
      )

getConfig :: IO Config
getConfig = execParser opts
  where
    opts =
      info
        (config <**> helper)
        ( fullDesc
            <> progDesc "A simple pomodoro that sets slack status"
            <> header "taut - un-Slack"
        )

slackCmd :: Text -> Text -> Text -> Text -> IO ()
slackCmd token chan cmd arg = do
  let opts =
        defaults
          & param "token" .~ [token]
          & param "channel" .~ [chan]
          & param "command" .~ ["/" <> cmd]
          & param "text" .~ [arg]
  res <- getWith opts "https://slack.com/api/chat.command"
  print res
  pure ()

taut :: IO ()
taut = do
  Config {..} <- getConfig
  let slackCmd' = slackCmd token chan
      noti (msg :: Text) = case notifier of
        Just n -> callCommand (Text.unpack n <> " " <> show msg)
        Nothing -> pure ()
      loop s = case s of
        Focus -> do
          slackCmd' "dnd" (Text.pack (show duration <> " minutes"))
          noti "Time to focus!"
          threadDelay (duration * minutes)
          loop Slack
        Slack -> do
          slackCmd' "dnd" "off"
          noti "Time to slack!"
          threadDelay ((60 - duration) * minutes)
          loop Focus
  loop startWith
  where
    seconds = 1000000
    minutes = 60 * seconds
