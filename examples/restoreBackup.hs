{-# LANGUAGE OverloadedStrings #-}

import Turtle

import qualified Control.Foldl as Fold

main = do
  -- Backup the old database
  inproc "mysqldump" ["-uroot", "myproject"] empty
      & output "revert.sql"

  let serverName = "lalala"
  let backupDir  = "/backups/db/myproject/"

  -- Obtain newest filename from server
  let query :: Shell Line
      query = inproc "ssh"
          [ serverName
          , format ("ls -Art "%fp%" | tail -n 1") backupDir
          ]
          empty
  result <- fold query Fold.head
  newestFileName <- case result of
      Nothing   -> die "Couldn't get backup path"
      Just text -> return (fromText (lineToText text))

  let backupFilePath = backupDir </> newestFileName
  let localFilePath  = "/tmp/" </> newestFileName

  -- Download the backup
  procs "rsync"
      [ "-avcz"
      , format (s%":"%fp) serverName backupFilePath
      , format fp localFilePath
      ]
      empty

  let dbName = "myproject-copy"

  -- Drop the old database
  shells ("mysqladmin -uroot drop -f " <> dbName) empty

  -- Restore the database from the downloaded backup
  inproc "zcat" [format fp localFilePath] empty
      & procs "mysql" ["-uroot", dbName]

  echo "Backup restored"
