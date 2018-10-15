DROP TABLE IF EXISTS `users`;
CREATE TABLE `users`
    ( `id`       INT(11) AUTO_INCREMENT PRIMARY KEY
    , `name`     TEXT
    , `birthday` TIMESTAMP NOT NULL
    , `num`      INT(11)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
