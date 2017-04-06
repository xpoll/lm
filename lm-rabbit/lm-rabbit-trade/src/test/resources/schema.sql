
CREATE TABLE `parana_order_extra` (
  `id`                    BIGINT          NOT NULL  AUTO_INCREMENT COMMENT '自增主键' ,
  `order_id`  BIGINT NOT NULL COMMENT '关联订单id',
  `order_type` SMALLINT NOT NULL COMMENT '订单类型',
  `channel` VARCHAR(64) NULL COMMENT '支付方式,alipay之类的',
  `trade_no`  VARCHAR(128)  NULL COMMENT '交易流水号',
  `batch_no`  VARCHAR(128)  NULL COMMENT '退款流水号',
  `payment_code`  VARCHAR(128)  NULL COMMENT '第三方交易流水号',
  `system_no`  VARCHAR(128)  NULL COMMENT '系统内部流水号',
  `trade_info`  VARCHAR(1024)  NULL COMMENT '收货地址',
  `buyer_notes` VARCHAR(256)  NULL COMMENT '买家留言',
  `seller_notes`  VARCHAR(256)  NULL COMMENT '卖家留言',
  `invoice` VARCHAR(1024) NULL COMMENT '发票信息',
  `paid_at` DATETIME  NULL COMMENT '支付时间',
  `buyer_cancel_at` DATETIME NULL COMMENT '买家取消订单时间',
  `seller_cancel_at`  DATETIME  NULL COMMENT '卖家取消订单时间',
  `deliver_at`  DATETIME  NULL COMMENT '发货时间',
  `done_at` DATETIME  NULL COMMENT '完成时间',
  `apply_at`  DATETIME  NULL COMMENT '申请时间',
  `check_at`  DATETIME  NULL COMMENT '审核时间',
  `buyer_deliver_at`  DATETIME  NULL COMMENT '买家发货时间',
  `seller_deliver_at` DATETIME  NULL COMMENT '卖家发货时间',
  `refund_at` DATETIME  NULL COMMENT '退款时间',
  `created_at`  DATETIME  NULL COMMENT '创建时间',
  `updated_at` DATETIME  NULL COMMENT '更新时间',
  PRIMARY KEY (`id`)
) COMMENT='订单关联信息';

CREATE TABLE `parana_user_vat_invoices` (
  `id`                    BIGINT          NOT NULL  AUTO_INCREMENT COMMENT '自增主键' ,
  `user_id`               BIGINT          NOT NULL  COMMENT '用户标识',
  `company_name`          VARCHAR(128)    NOT NULL  COMMENT '公司名称',
  `tax_register_no`       VARCHAR(32)     NOT NULL  COMMENT '税务登记号',
  `register_address`      VARCHAR(128)    NOT NULL  COMMENT '注册地址',
  `register_phone`        VARCHAR(16)     NOT NULL  COMMENT '注册电话',
  `register_bank`         VARCHAR(128)    NOT NULL  COMMENT '注册银行',
  `bank_account`          VARCHAR(32)     NOT NULL  COMMENT '银行帐号',
  `tax_certificate`       VARCHAR(256)    NULL      COMMENT '税务登记证',
  `taxpayer_certificate`  VARCHAR(256)    NULL      COMMENT '一般纳税人证书',
  `created_at`            DATETIME        NULL      COMMENT '创建时间',
  `updated_at`            DATETIME        NULL      COMMENT '更新时间',
  PRIMARY KEY (`id`)
)COMMENT='用户增值税发票表';
CREATE UNIQUE INDEX idx_parana_uvi_user_id_uniq on `parana_user_vat_invoices`(`user_id`);




-- 中间表 服务于结算中心

-- 订单资金流水明细
DROP TABLE IF EXISTS `parana_order_money_flows`;

CREATE TABLE `parana_order_money_flows` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `order_id`                BIGINT        NOT NULL                   COMMENT '关联订单id',
  `order_type`              SMALLINT      NOT NULL                   COMMENT '订单类型',
  `seller_id`               BIGINT        NOT NULL                   COMMENT '商家id',
  `seller_name`             VARCHAR(64)   NULL                       COMMENT '商家名称',
  `shop_id`                 BIGINT        NOT NULL                   COMMENT '店铺id',
  `shop_name`               VARCHAR(64)   NULL                       COMMENT '商家名称',
  `buyer_id`                BIGINT        NOT NULL                   COMMENT '买家id',
  `buyer_name`              VARCHAR(64)   NULL                       COMMENT '买家名称',
  `type`                    SMALLINT      NOT NULL                   COMMENT '业务类型 1:支付,2:售中退款,3:售后退款',
  `pay_type`                SMALLINT      NOT NULL                   COMMENT '支付方式 1:在线支付,2:货到付款,3:积分',
  `channel`                 VARCHAR(64)   NOT NULL                   COMMENT '支付渠道',
  `system_no`               VARCHAR(64)   NULL                       COMMENT '系统内部流水号',
  `trade_no`                VARCHAR(64)   NULL                       COMMENT '支付流水号,供支付使用',
  `payment_code`            VARCHAR(64)   NULL                       COMMENT '第三方流水号',
  `batch_no`                VARCHAR(64)   NULL                       COMMENT '退款批次号',
  `fee`                     BIGINT        NOT NULL                   COMMENT '金额(实付或者退款)',
  `memo`                    VARCHAR(64)   NULL                       COMMENT '备注信息',
  `trade_at`                DATETIME      NOT NULL                   COMMENT '付款或退款时间',
  `is_settlemented`         BIT           NOT NULL DEFAULT 0         COMMENT '是否已生成对应的结算单',
  `created_at`  DATETIME  NULL COMMENT '创建时间',
  `updated_at` DATETIME  NULL COMMENT '更新时间',
  PRIMARY KEY (`id`)
) COMMENT='订单收支明细信息';


DROP TABLE IF EXISTS `parana_order_finish_infos`;

CREATE TABLE IF NOT EXISTS `parana_order_finish_infos` (
  `id`                      BIGINT        NOT NULL    AUTO_INCREMENT COMMENT '自增主键',
  `order_id`                BIGINT        NOT NULL                   COMMENT '订单id',
  `seller_id`               BIGINT        NOT NULL                   COMMENT '商家id',
  `seller_name`             VARCHAR(64)   NULL                       COMMENT '商家名称',
  `shop_id`                 BIGINT        NOT NULL                   COMMENT '店铺id',
  `shop_name`               VARCHAR(64)   NULL                       COMMENT '商家名称',
  `buyer_id`                BIGINT        NOT NULL                   COMMENT '买家id',
  `buyer_name`              VARCHAR(64)   NULL                       COMMENT '买家名称',
  `order_type`              SMALLINT      NOT NULL                   COMMENT '订单类型',
  `type`                    SMALLINT      NOT NULL                   COMMENT '交易类型 1:普通,2:预售',
  `pay_type`                SMALLINT      NOT NULL                   COMMENT '支付方式 1:在线支付,2:货到付款,3:积分',
  `order_status`            SMALLINT      NOT NULL                   COMMENT '订单交易状态',
  `system_no`               VARCHAR(64)   NULL                       COMMENT '系统内部流水号',
  `origin_fee`              BIGINT        NULL                       COMMENT '原价',
  `fee`                     BIGINT        NULL                       COMMENT '实付金额',
  `discount`                BIGINT        NULL                       COMMENT '优惠金额',
  `ship_fee`                BIGINT        NULL                       COMMENT '运费',
  `ship_fee_discount`       BIGINT        NULL                       COMMENT '运费优惠金额',
  `integral`                BIGINT        NULL                       COMMENT '积分减免金额',
  `balance`                 BIGINT        NULL                       COMMENT '余额减免金额',
  `sale_tax`                BIGINT        NULL                       COMMENT '消费税',
  `ship_fee_sale_tax`       BIGINT        NULL                       COMMENT '运费中包含的消费税',
  `is_settlemented`         BIT           NOT NULL DEFAULT 0         COMMENT '是否已生成对应的结算单',
  `finished_at`             DATETIME      NULL                       COMMENT '订单结束时间(计算各种费用)',
  `created_at`              DATETIME      NULL                       COMMENT '创建时间',
  `updated_at`              DATETIME      NULL                       COMMENT '修改时间',
  PRIMARY KEY (`id`));

CREATE INDEX idx_pofi_is_settlemented ON parana_order_finish_infos(is_settlemented);
CREATE INDEX idx_pofi_order_id ON parana_order_finish_infos(order_id);

