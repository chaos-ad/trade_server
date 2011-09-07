CREATE TABLE `history` (
  `Symbol`      varchar(15)         NOT NULL,
  `Period`      int(10) unsigned    NOT NULL,
  `Timestamp`   timestamp           NOT NULL DEFAULT 0,
  `Open`        double              NOT NULL,
  `High`        double              NOT NULL,
  `Low`         double              NOT NULL,
  `Close`       double              NOT NULL,
  `Volume`      double              NOT NULL,
  PRIMARY KEY (`Symbol`,`Period`,`Timestamp`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8; 
