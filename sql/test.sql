DROP TABLE IF EXISTS `users`;
CREATE TABLE `users`
    ( `id`       INT(11) AUTO_INCREMENT PRIMARY KEY
    , `name`     TEXT      NOT NULL
    , `birthday` TIMESTAMP NOT NULL
    , `weight`   DOUBLE    NOT NULL
    , `age`      INT(11)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
