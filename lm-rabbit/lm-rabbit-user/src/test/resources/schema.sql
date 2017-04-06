-- 卖家主账户角色表: galaxy_main_seller_roles

CREATE TABLE `galaxy_main_seller_roles` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `name` VARCHAR(40) NULL COMMENT '用户名',
  `desc` VARCHAR(32) NULL COMMENT '角色描述',
  `status` SMALLINT NULL COMMENT '0. 未生效(冻结), 1. 生效, -1. 删除',
  `extra_json` VARCHAR(1024) NULL COMMENT '用户额外信息,建议json字符串',
  `allow_json` VARCHAR(1024) NULL COMMENT '',
  `created_at` datetime NOT NULL,
  `updated_at` datetime NOT NULL,
  PRIMARY KEY (`id`)
) COMMENT='卖家主账户角色表';

-- 卖家角色表: galaxy_seller_roles

CREATE TABLE `galaxy_seller_roles` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `name` VARCHAR(40) NULL COMMENT '用户名',
  `desc` VARCHAR(32) NULL COMMENT '角色描述',
  `shop_id` bigint(20) NULL COMMENT '店铺 ID',
  `app_key` VARCHAR(16) NOT NULL COMMENT '角色所属',
  `status` SMALLINT NULL COMMENT '0. 未生效(冻结), 1. 生效, -1. 删除',
  `extra_json` VARCHAR(1024) NULL COMMENT '用户额外信息,建议json字符串',
  `allow_json` VARCHAR(1024) NULL COMMENT '',
  `created_at` datetime NOT NULL,
  `updated_at` datetime NOT NULL,
  PRIMARY KEY (`id`)
) COMMENT='卖家角色表';
CREATE INDEX idx_seller_roles_shop_id ON `galaxy_seller_roles` (`shop_id`);

-- 运营角色表: galaxy_operator_roles

CREATE TABLE `galaxy_operator_roles` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `name` VARCHAR(40) NULL COMMENT '用户名',
  `desc` VARCHAR(32) NULL COMMENT '角色描述',
  `app_key` VARCHAR(16) NOT NULL COMMENT '角色所属',
  `status` SMALLINT NULL COMMENT '0. 未生效(冻结), 1. 生效, -1. 删除',
  `extra_json` VARCHAR(1024) NULL COMMENT '用户额外信息,建议json字符串',
  `allow_json` VARCHAR(1024) NULL COMMENT '',
  `created_at` datetime NOT NULL,
  `updated_at` datetime NOT NULL,
  PRIMARY KEY (`id`)
) COMMENT='运营角色表';

-- 用户运营表: galaxy_user_operators

CREATE TABLE `galaxy_user_operators` (
  `id`         BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
  `user_id`    BIGINT          NULL COMMENT '用户 ID',
  `role_id`    BIGINT          NULL COMMENT '运营角色 ID',
  `status`     TINYINT         NULL COMMENT '运营状态',
  `extra_json` VARCHAR(1024)   NULL COMMENT '运营额外信息, 建议json字符串',
  `created_at` DATETIME        NOT NULL,
  `updated_at` DATETIME        NOT NULL,
  PRIMARY KEY (`id`)
) COMMENT = '用户运营表';
CREATE INDEX idx_user_operator_user_id ON `galaxy_user_operators` (`user_id`);
CREATE INDEX idx_user_operator_role_id ON `galaxy_user_operators` (`role_id`);

-- 商家子账户表: galaxy_user_sub_sellers

CREATE TABLE `galaxy_user_sub_sellers` (
  `id`         BIGINT(20) UNSIGNED NOT NULL AUTO_INCREMENT,
  `user_id`    BIGINT(20)          NULL COMMENT '用户 ID',
  `user_name`  VARCHAR(64)         NULL COMMENT '用户名 (冗余)',
  `shop_id`    BIGINT(20)          NULL COMMENT '店铺 ID',
  `status`     TINYINT             NULL COMMENT '状态',
  `roles_json` VARCHAR(1024)       NULL COMMENT '角色 ID 列表',
  `extra_json` VARCHAR(1024)       NULL COMMENT '用户额外信息, 建议json字符串',
  `created_at` DATETIME            NOT NULL,
  `updated_at` DATETIME            NOT NULL,
  PRIMARY KEY (`id`)
) COMMENT = '商家子账户表';
CREATE INDEX idx_user_sub_seller_user_id ON `galaxy_user_sub_sellers` (`user_id`);
CREATE INDEX idx_user_sub_seller_sub_id ON `galaxy_user_sub_sellers` (shop_id);


-- 用户设备信息表 galaxy_user_devices

create table `galaxy_user_devices` (
  `id` bigint UNSIGNED NOT NULL AUTO_INCREMENT,
  `user_id` bigint null COMMENT '用户ID',
  `user_name` VARCHAR(64) COMMENT '用户名',
  `device_token` VARCHAR(128) COMMENT '',
  `device_type` VARCHAR(128) COMMENT '',
  `created_at` datetime NULL ,
  `updated_at` datetime NULL ,
   PRIMARY KEY (`id`)
) COMMENT = '用户设备信息表';
CREATE INDEX idx_user_devices_user_id ON `galaxy_user_devices` (`user_id`);
CREATE INDEX idx_user_devices_token ON `galaxy_user_devices` (`device_token`);


-- 商家子账户表: galaxy_user_sub_sellers

CREATE TABLE `galaxy_user_sellers` (
  `id`         BIGINT(20) UNSIGNED NOT NULL AUTO_INCREMENT,
  `user_id`    BIGINT(20)          NULL COMMENT '用户 ID',
  `user_name`  VARCHAR(64)         NULL COMMENT '用户名 (冗余)',
  `shop_id`    BIGINT(20)          NULL COMMENT '店铺 ID',
  `shop_name`  VARCHAR(64)         NULL COMMENT '店铺名 (冗余)',
  `status`     TINYINT             NULL COMMENT '状态',
  `extra_json` VARCHAR(4096)       NULL COMMENT '用户额外信息, 建议json字符串',
  `created_at` DATETIME            NOT NULL,
  `updated_at` DATETIME            NOT NULL,
  PRIMARY KEY (`id`)
) COMMENT = '商家子账户表';
CREATE INDEX idx_user_seller_user_id ON `galaxy_user_sellers` (`user_id`);
CREATE INDEX idx_user_seller_sub_id ON `galaxy_user_sellers` (shop_id);

-- 业务表
CREATE TABLE `galaxy_businesses` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '自增主键',
  `business_name` VARCHAR (256) NOT NULL COMMENT '业务名称',
  `type` INT(6) DEFAULT NULL COMMENT '业务类型',
  `extra_json` varchar(1024) DEFAULT NULL COMMENT '业务,建议json字符串',
  `img` VARCHAR(256) DEFAULT NULL COMMENT '图片',
  `description` VARCHAR(256) DEFAULT NULL COMMENT '描述',
  `business_link` VARCHAR(256) DEFAULT NULL COMMENT '链接',
  `created_at` datetime DEFAULT NULL COMMENT '创建时间',
  `updated_at` datetime DEFAULT NULL COMMENT '修改时间',
  PRIMARY KEY (`id`),
  KEY `idx_business_business_name` (`business_name`),
  KEY `idx_business_type` (`type`)
) COMMENT='业务';

-- 新闻公告表
CREATE TABLE `parana_notice` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '自增主键',
  `title` varchar(128) NOT NULL COMMENT '标题',
  `level` smallint(6) NOT NULL COMMENT '1->商家, 2->平台',
  `type` smallint(6) NOT NULL COMMENT '公共类型1/新闻2/公告',
  `shop_id` bigint(20) DEFAULT NULL COMMENT '店铺id',
  `status` smallint(6) NOT NULL COMMENT '状态，0->初始化（未发布）,1->已发布2->停止',
  `context` varchar(2048) NOT NULL COMMENT '公告详情',
  `creator_id` bigint(20) DEFAULT NULL COMMENT '创建人id',
  `creator_name` varchar(128) DEFAULT NULL COMMENT '创建人名称',
  `start_at` datetime DEFAULT NULL COMMENT '生效时间',
  `end_at` datetime DEFAULT NULL COMMENT '失效时间',
  `pic_url` varchar(128) DEFAULT NULL COMMENT '图片地址',
  `extra` varchar(512) DEFAULT NULL COMMENT '额外数据信息',
  `created_at` datetime NOT NULL COMMENT '创建时间',
  `updated_at` datetime NOT NULL COMMENT '更新时间',
  `summary` varchar(256) DEFAULT NULL COMMENT '新闻摘要',
  PRIMARY KEY (`id`)
);
