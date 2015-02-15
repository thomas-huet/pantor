CREATE TABLE file(
  torrent TEXT NOT NULL,
  name TEXT NOT NULL,
  size BIGINT NOT NULL,
  UNIQUE(torrent, name)
);

CREATE INDEX file_torrent ON file(torrent);
