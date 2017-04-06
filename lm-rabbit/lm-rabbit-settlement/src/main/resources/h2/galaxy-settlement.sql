
-- -----------------------------------------------------
-- Table `parana_settlements_sum_of_daily` 订单帐务日汇总
-- -----------------------------------------------------
DROP TABLE IF EXISTS `parana_settlements_sum_of_dailys`;

CREATE TABLE IF NOT EXISTS `parana_settlements_sum_of_dailys` (
  `id`                      BIGINT        NOT NULL    AUTO_INCREMENT COMMENT '自增主键',
  `order_count`             INTEGER       NULL        COMMENT '订单数',
  `fee`                     BIGINT        NOT NULL    COMMENT '订单金额（实付款 正数）',
  `origin_fee`              BIGINT        NOT NULL    COMMENT '原价（商品价格和 正数）',
  `seller_discount`      BIGINT        NOT NULL    COMMENT '商家营销支出（负数）',
  `ship_fee`                BIGINT        NOT NULL    COMMENT '运费（正数）',
  `receivable`              BIGINT        NOT NULL    COMMENT '应收货款（正数）',
  `platform_discount`      BIGINT        NOT NULL    COMMENT '平台营销贴现（正数）',
  `refund_fee`       BIGINT        NULL        COMMENT '交易总支出(退款 负数）',
  `commission`              BIGINT        NULL        COMMENT '平台抽佣（正数）',
  `integral`           BIGINT        NULL        COMMENT '平台积分收入（正数）',
  `third_party_fee`         BIGINT        NULL        COMMENT '支付平台佣金（负数）',
  `sale_tax`                BIGINT        NULL        COMMENT '消费税',
  `sum_at`                  DATETIME      NULL        COMMENT '汇总时间',
  `created_at`              DATETIME      NULL        COMMENT '创建时间',
  `updated_at`              DATETIME      NULL        COMMENT '修改时间',
  PRIMARY KEY (`id`));

CREATE INDEX idx_essod_created_at ON parana_settlements_sum_of_dailys(created_at);


-- -----------------------------------------------------
-- Table `parana_settlements_sum_of_shop_daily` 订单账务商家日汇总
-- -----------------------------------------------------
DROP TABLE IF EXISTS `parana_settlements_sum_of_shop_dailys`;

CREATE TABLE IF NOT EXISTS `parana_settlements_sum_of_shop_dailys` (
  `id`                      BIGINT        NOT NULL    AUTO_INCREMENT COMMENT '自增主键',
  `seller_id`               BIGINT        NOT NULL    COMMENT '商家id',
  `seller_name`             VARCHAR(64)   NULL        COMMENT '商家名称',
  `shop_id`                 BIGINT        NOT NULL    COMMENT '店铺id',
  `shop_name`               VARCHAR(64)   NULL        COMMENT '商家名称',
  `order_count`             INTEGER       NULL        COMMENT '订单数',
  `fee`                     BIGINT        NOT NULL    COMMENT '实收金额（正数）',
  `origin_fee`              BIGINT        NOT NULL    COMMENT '原价（商品价格和 正数）',
  `seller_discount`      BIGINT        NOT NULL    COMMENT '商家营销支出（负数）',
  `ship_fee`                BIGINT        NOT NULL    COMMENT '运费（正数）',
  `receivable`              BIGINT        NOT NULL    COMMENT '应收货款 （正数）',
  `platform_discount`      BIGINT        NOT NULL    COMMENT '平台营销贴现（正数）',
  `refund_fee`       BIGINT        NULL        COMMENT '交易总支出(退款 负数）',
  `commission`              BIGINT        NULL        COMMENT '平台抽佣(根据具体的业务规则 负数）',
  `integral`           BIGINT        NULL        COMMENT '平台积分收入（正数）',
  `third_party_fee`         BIGINT        NULL        COMMENT '支付平台佣金（负数）',
  `sale_tax`                BIGINT        NULL        COMMENT '消费税',
  `sum_at`                  DATETIME      NULL        COMMENT '汇总时间',
  `created_at`              DATETIME      NULL        COMMENT '创建时间',
  `updated_at`              DATETIME      NULL        COMMENT '修改时间',
  PRIMARY KEY (`id`));

CREATE INDEX idx_essosd_created_at ON parana_settlements_sum_of_shop_dailys(created_at);

