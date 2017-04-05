
-- 订单支付相关信息表: parana_pays 对应一次支付，也许是合并支付也许非合并支付

drop table if exists `parana_pays`;

create table if not exists `parana_pays` (
  `id`                bigint    unsigned  not null  auto_increment ,
  `order_id`          BIGINT        NULL      COMMENT '订单号（非合并支付）',
  `order_ids`         TEXT          NULL      COMMENT '订单号（仅合并支付）,以逗号分隔，最多10个',
  `buyer_id`          BIGINT        NULL      COMMENT '买家id',
  `system_no`         VARCHAR(32)   NULL      COMMENT '系统内部流水',
  `order_type`        TINYINT       NULL      COMMENT '订单类型',
  `paid_type`         TINYINT       NULL      COMMENT '支付方式 0:货到付款 1:在线支付 2:分期支付',
  `paid_status`       TINYINT       NULL      COMMENT '支付状态 0:未支付 1:已支付 -1:支付超时',
  `merge_paid`        BOOLEAN       NULL      COMMENT '是否合并支付',
  `expired_at`        DATETIME      NULL      COMMENT '交易超时时间',
  `paid_at`           DATETIME      NULL      COMMENT '支付完成时间',
  `stage`             int           NOT NULL  COMMENT '分几期',
  `current_stage`     int           NULL      COMMENT '当前支付到第几期',
  `should_fee`        int           null      comment '应付款',
  `already_fee`       int           null      comment '已付款',
  `subject`           VARCHAR(128)  NULL      COMMENT '标题',
  `created_at`        datetime      not null  comment '创建时间',
  `updated_at`        datetime      not null  comment '更新时间',
  PRIMARY KEY (`id`)
);
create index idx_parana_pays_order_id on parana_pays (order_id);

-- 分期阶段表: parana_pay_stages 冗余了支付请求相关信息 和第三方手续费（事先写入包括合并支付的拆分）

drop table if exists `parana_pay_stages`;

create table if not exists `parana_pay_stages` (
  `id`              bigint    unsigned  not null  auto_increment ,
  `pay_id`          bigint    NOT NULL  comment '支付id',
  `current_stage`   int       NOT NULL  COMMENT '当前第几期',
  `paid_status`     TINYINT       NULL  COMMENT '支付状态 0:未支付 1:已支付 -1:支付超时',
  `payment_code`    VARCHAR(64)   NULL  COMMENT '支付交易流水（第三方）',
  `channel`         varchar(256)  null  comment '支付渠道',
  `fee`             int           NULL  comment '本期应付款（即本次支付金额）',
  `content`         VARCHAR(1023) NULL  COMMENT '说明',
  `system_no`       VARCHAR(32)   NULL  COMMENT '系统内部流水',
  `paid_at`         DATETIME      NULL  COMMENT '支付完成时间',
  `expired_at`      DATETIME      NULL  COMMENT '交易超时时间(支付)',
  `repay_at`        datetime      NULL  comment '最大还款时间',
  `created_at`      datetime      NOT NULL  comment '创建时间',
  `updated_at`      datetime      NOT NULL  comment '更新时间',
  PRIMARY KEY (`id`)
);
create index idx_parana_pay_stages_pay_id on parana_pay_stages (pay_id);


-- 支付渠道信息表: parana_pay_channels

drop table if exists `parana_pay_channels`;

create table if not exists `parana_pay_channels` (
  `id`              bigint        unsigned  not null  auto_increment ,
  `stage_id`        bigint        NULL  comment '分期阶段id',
  `status`          TINYINT       NULL      COMMENT '状态 0:待处理 1:已处理 -1:超时',
  `trade_no`        VARCHAR(32)   NULL      COMMENT '交易流水（供第三方支付平台使用）',
  `payment_code`    VARCHAR(64)   NULL      COMMENT '支付交易流水（第三方）',
  `channel`         varchar(64)   null      comment '支付渠道',
  `batch_no`        VARCHAR(64)   NULL      COMMENT '退款批次号',
  `refund_order_id` bigint        NULL      comment '退款单id',
  `order_type`      SMALLINT      NULL      COMMENT '订单类型',
  `type`            SMALLINT      NULL      COMMENT '类型, 1.支付 2.退款',
  `business_type`   SMALLINT      NULL      COMMENT '业务类型, 1.正向 2.售中退款 3.售后退款',
  `fee`             int           NULL      comment '金额',
  `extra`           TEXT          NULL      COMMENT '扩展字段',
  `description`     VARCHAR(256)  NULL      COMMENT '说明',
  `is_created_detail`     TINYINT NOT  NULL COMMENT '是否已结算 0未创建、 1:已创建支付渠道汇总明细',
  `paid_at`         datetime      NULL      comment '支付完成时间',
  `refund_at`       datetime      NULL      comment '退款完成时间',
  `created_at`      datetime      NOT NULL  comment '创建时间',
  `updated_at`      datetime      NOT NULL  comment '更新时间',
  PRIMARY KEY (`id`)
);
create index idx_parana_pay_channels_stage_id on parana_pay_channels (stage_id);
create index idx_parana_pay_channels_trade_no on parana_pay_channels (trade_no);
create index idx_parana_pay_channels_batch_no on parana_pay_channels (batch_no);
create index idx_parana_pay_channels_type on parana_pay_channels (type);
create index idx_parana_pay_channels_channel on parana_pay_channels (channel);


-- 站点或店铺拥有的支付渠道信息表

drop table if exists `parana_owner_pay_channels`;

CREATE TABLE `parana_owner_pay_channels` (
  `id`                bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `owner_id`         bigint(20) NOT NULL COMMENT 'id',
  `owner_name`       VARCHAR(64)NULL     COMMENT '名称',
  `type`             TINYINT    NOT NULL     COMMENT '类型 1:站点 2:店铺',
  `channel`           TEXT DEFAULT NULL   COMMENT '渠道逗号分隔',
  `created_at`        datetime DEFAULT NULL,
  `updated_at`        datetime DEFAULT NULL,
  PRIMARY KEY (`id`)
);
CREATE INDEX parana_owner_pay_channels_idx_user_id ON `parana_owner_pay_channels` (owner_id);


-- 异常支付追踪表: parana_exception_pay_tracks

drop table if exists `parana_exception_pay_tracks`;

create table if not exists `parana_exception_pay_tracks` (
  `id`              bigint        unsigned  not null  auto_increment ,
  `trade_no`        VARCHAR(32)   NULL      COMMENT '交易流水（供第三方支付平台使用）',
  `payment_code`    VARCHAR(64)   NULL      COMMENT '支付交易流水（第三方）',
  `channel`         varchar(64)   null      comment '支付渠道',
  `type`            TINYINT    NOT NULL     COMMENT '类型 1:超时支付订单已关闭 2:重复支付',
  `fee`             int           NULL      comment '金额',
  `status`          TINYINT       NULL      COMMENT '处理状态 0:未处理 1:已处理 -1:已关闭',
  `description`     VARCHAR(256)  NULL      COMMENT '说明',
  `paid_at`         datetime      NULL      comment '支付完成时间',
  `created_at`      datetime      NOT NULL  comment '创建时间',
  `updated_at`      datetime      NOT NULL  comment '更新时间',
  PRIMARY KEY (`id`)
);
CREATE INDEX idx_ptpts_trade_no ON parana_exception_pay_tracks (trade_no);
CREATE INDEX idx_ptpts_payment_code ON parana_exception_pay_tracks (payment_code);


-- 第三方账务


-- -----------------------------------------------------
-- Table `parana_alipay_trans`  支付宝交易对账数据
-- -----------------------------------------------------
DROP TABLE IF EXISTS `parana_alipay_trans`;
CREATE TABLE IF NOT EXISTS `parana_alipay_trans` (
  `id`                          BIGINT        NOT NULL   AUTO_INCREMENT COMMENT '自增主键',
  `balance`                     VARCHAR(32)   NULL       COMMENT '账户余额',
  `bank_account_name`           VARCHAR(32)   NULL       COMMENT '银行账户名称',
  `bank_account_no`             VARCHAR(32)   NULL       COMMENT '银行账户',
  `bank_name`                   VARCHAR(64)   NULL       COMMENT '银行名',
  `buyer_name`                  VARCHAR(127)  NULL       COMMENT '买家姓名',
  `buyer_account`               VARCHAR(32)   NULL       COMMENT '买家账户',
  `currency`                    VARCHAR(16)   NULL       COMMENT '货币代码(156:人民币)',
  `deposit_bank_no`             VARCHAR(32)   NULL       COMMENT '充值网银流水',
  `income`                      VARCHAR(32)   NULL       COMMENT '收入金额',
  `iw_account_log_id`           VARCHAR(32)   NULL       COMMENT '帐务流水',
  `memo`                        VARCHAR(127)  NULL       COMMENT '备注信息',
  `merchant_out_order_no`       VARCHAR(64)   NULL       COMMENT '外部交易编号（订单号）',
  `other_account_email`         VARCHAR(127)  NULL       COMMENT '帐务对方邮箱',
  `other_account_fullname`      VARCHAR(127)  NULL       COMMENT '帐务对方全称',
  `other_user_id`               VARCHAR(32)   NULL       COMMENT '帐务对方支付宝用户号',
  `outcome`                     VARCHAR(32)   NULL       COMMENT '支出金额',
  `partner_id`                  VARCHAR(32)   NULL       COMMENT '合作者身份id',
  `seller_account`              VARCHAR(32)   NULL       COMMENT '买家支付宝人民币支付帐号(user_id+0156)',
  `seller_fullname`             VARCHAR(64)   NULL       COMMENT '卖家姓名',
  `service_fee`                 VARCHAR(32)   NULL       COMMENT '交易服务费',
  `service_fee_ratio`           VARCHAR(16)   NULL       COMMENT '交易服务费率',
  `total_fee`                   VARCHAR(32)   NULL       COMMENT '交易总金额',
  `trade_no`                    VARCHAR(32)   NULL       COMMENT '支付宝交易流水',
  `trade_refund_amount`         VARCHAR(32)   NULL       COMMENT '累计退款金额',
  `trans_account`               VARCHAR(32)   NULL       COMMENT '帐务本方支付宝人民币资金帐号(user_id+0156)',
  `trans_code_msg`              VARCHAR(16)   NULL       COMMENT '业务类型',
  `trans_date`                  VARCHAR(32)   NULL       COMMENT '交易发生日期',
  `trans_out_order_no`          VARCHAR(32)   NULL       COMMENT '商户订单号',
  `sub_trans_code_msg`          VARCHAR(32)   NULL       COMMENT '子业务类型代码，详见文档',
  `sign_product_name`           VARCHAR(32)   NULL       COMMENT '签约产品',
  `rate`                        VARCHAR(16)   NULL       COMMENT '费率',
  `trade_at`                    DATETIME      NULL       COMMENT '交易时间',
  `created_at`                  DATETIME      NULL       COMMENT '创建时间',
  `updated_at`                  DATETIME      NULL       COMMENT '修改时间',
  PRIMARY KEY (`id`)
);
CREATE INDEX idx_eat_iw_account_log_id ON parana_alipay_trans (iw_account_log_id);
CREATE INDEX idx_eat_trans_no ON parana_alipay_trans (trade_no);
CREATE INDEX idx_eat_merchant_no ON parana_alipay_trans (merchant_out_order_no);



-- 微信支付账务明细
CREATE TABLE `parana_wechatpay_trans` (
  `id` BIGINT NOT NULL AUTO_INCREMENT,
  `transaction_id`    VARCHAR(32)   NOT NULL COMMENT '微信支付订单号',
  `out_trade_no`      VARCHAR(32)   NOT NULL COMMENT '商户系统的订单号，与请求一致',
  `trade_status`      VARCHAR(16)   NULL COMMENT '交易状态 SUCCESS FAIL',
  `trade_time`        VARCHAR(32)   NOT NULL COMMENT '交易时间',
  `appid`             VARCHAR(64)   NULL COMMENT '公众账号 ID',
  `mch_id`            VARCHAR(32)   NULL COMMENT '微信支付分配的商户号',
  `sub_mch_id`        VARCHAR(32)   NULL COMMENT '微信支付分配的子商户号， 受理模式下必填',
  `device_info`       VARCHAR(128)  NULL COMMENT '微信支付分配的终端设备号',
  `open_id`           VARCHAR(128)  NULL COMMENT '用户标识',
  `trade_type`        VARCHAR(16)   NULL COMMENT 'JSAPI、NATIVE、APP 交易类型',
  `bank_type`         VARCHAR(32)   NULL COMMENT '付款银行，采用字符串类型的银行标识',
  `fee_type`          VARCHAR(16)   NULL COMMENT '货币种类，符合ISO 4217 标准的三位字母代码，默认人民币：CNY',
  `total_fee`         VARCHAR(64)   NOT NULL COMMENT '本次交易金额 元',
  `coupon_fee`        VARCHAR(64)   NULL  COMMENT '现金券金额 元',
  `refund_apply_date`  VARCHAR(32)   NULL COMMENT '退款申请时间',
  `refund_success_date` VARCHAR(32)   NULL COMMENT '退款成功时间',
  `refund_id`         VARCHAR(32)   NULL COMMENT '微信退款单号',
  `out_refund_no`     VARCHAR(32)   NULL COMMENT '商户退款单号',
  `refund_fee`        VARCHAR(64)   NULL COMMENT '退款金额',
  `coupon_refund_fee` VARCHAR(64)   NULL COMMENT '现金券退款金额',
  `refund_channel`    VARCHAR(16)   NULL COMMENT '退款类型 ORIGINAL—原路退款 BALANCE—退回到余额',
  `refund_status`     VARCHAR(16)   NULL COMMENT '退款状态：SUCCES—退款成功 FAIL—退款失败 PROCESSING—退款处理中 NOTSURE—未确定，需要商户 原退款单号重新发起 CHANGE—转入代发，退款到 银行发现用户的卡作废或者冻结了，导致原路退款银行 卡失败，资金回流到商户的现金帐号，需要商户人工干 预，通过线下或者财付通转 账的方式进行退款。',
  `body`              VARCHAR(128)  NULL COMMENT '商品名称',
  `attach`            TEXT          NULL COMMENT '商户数据包 附加数据',
  `poundage_fee`      VARCHAR(64)   NULL COMMENT '手续费',
  `rate`              VARCHAR(16)   NULL COMMENT '费率',
  `bank_order_no`     VARCHAR(64)   NULL COMMENT '银行订单号',
  `trade_info`        TEXT          NULL COMMENT '交易说明',
  `trade_at`                    DATETIME      NULL       COMMENT '交易时间',
  PRIMARY KEY (`id`)
)COMMENT = '微信支付账务明细';




-- unionpay对账数据

DROP TABLE IF EXISTS `parana_unionpay_trans`;

CREATE TABLE IF NOT EXISTS `parana_unionpay_trans` (
  `id`                          BIGINT        NOT NULL   AUTO_INCREMENT COMMENT '自增主键',
  `transaction_code`            VARCHAR(32)   NULL       COMMENT '交易码',
  `acq_ins_code`                VARCHAR(32)   NULL       COMMENT '代理机构标识码',
  `send_code`                   VARCHAR(32)   NULL       COMMENT '发送机构标识码',
  `trace_no`                    VARCHAR(32)   NULL       COMMENT '系统跟踪号',
  `pay_card_no`                 VARCHAR(32)   NULL       COMMENT '帐号',
  `txn_amt`                     VARCHAR(32)   NULL       COMMENT '交易金额',
  `mer_cat_code`                VARCHAR(32)   NULL       COMMENT '商户类别',
  `term_type`                   VARCHAR(32)   NULL       COMMENT '终端类型',
  `query_id`                    VARCHAR(32)   NULL       COMMENT '查询流水号',
  `type`                        VARCHAR(32)   NULL       COMMENT '支付方式（旧）',
  `order_id`                    VARCHAR(32)   NULL       COMMENT '商户订单号',
  `pay_card_type`               VARCHAR(32)   NULL       COMMENT '支付卡类型',
  `original_trace_no`           VARCHAR(32)   NULL       COMMENT '原始交易的系统跟踪号',
  `original_time`                VARCHAR(32)   NULL       COMMENT '原始交易日期时间',
  `third_party_fee`             VARCHAR(32)   NULL       COMMENT '商户手续费',
  `settle_amount`               VARCHAR(32)   NULL       COMMENT '结算金额',
  `pay_type`                    VARCHAR(32)   NULL       COMMENT '支付方式',
  `company_code`                VARCHAR(32)   NULL       COMMENT '集团商户代码',
  `txn_type`                    VARCHAR(32)   NULL       COMMENT '交易类型',
  `txn_sub_type`                VARCHAR(32)   NULL       COMMENT '交易子类',
  `biz_type`                    VARCHAR(32)   NULL       COMMENT '业务类型',
  `acc_type`                    VARCHAR(32)   NULL       COMMENT '帐号类型',
  `bill_type`                  VARCHAR(32)   NULL       COMMENT '账单类型',
  `bill_no`                     VARCHAR(32)   NULL       COMMENT '账单号码  ',
  `interact_mode`               VARCHAR(32)   NULL       COMMENT '交互方式',
  `orig_qry_id`                  VARCHAR(32)   NULL       COMMENT '原交易查询流水号',
  `mer_id`                      VARCHAR(32)   NULL       COMMENT '商户代码',
  `divide_type`                 VARCHAR(32)   NULL       COMMENT '分账入账方式',
  `sub_mer_id`                  VARCHAR(32)   NULL       COMMENT '二级商户代码',
  `sub_mer_abbr`                VARCHAR(32)   NULL       COMMENT '二级商户简称',
  `divide_amount`               VARCHAR(32)   NULL       COMMENT '二级商户分账入账金额',
  `clearing`                    VARCHAR(32)   NULL       COMMENT '清算净额',
  `term_id`                     VARCHAR(32)   NULL       COMMENT '终端号',
  `mer_reserved`                VARCHAR(32)   NULL       COMMENT '商户自定义域',
  `discount`                    VARCHAR(32)   NULL       COMMENT '优惠金额',
  `invoice`                     VARCHAR(32)   NULL       COMMENT '发票金额',
  `addition_third_party_fee`    VARCHAR(32)   NULL       COMMENT '分期付款附加手续费',
  `stage`                       VARCHAR(32)   NULL       COMMENT '分期付款期数',
  `transaction_media`           VARCHAR(32)   NULL       COMMENT '交易介质',
  `original_order_id`           VARCHAR(32)   NULL       COMMENT '原始交易订单号',
  `txn_time`                    DATETIME      NULL       COMMENT '交易时间',
  `created_at`                  DATETIME      NULL       COMMENT '创建时间',
  `updated_at`                  DATETIME      NULL       COMMENT '修改时间',
  PRIMARY KEY (`id`)
);
CREATE INDEX idx_put_query_id ON parana_unionpay_trans (query_id);


-- -----------------------------------------------------
-- Table `parana_kjtpay_trans`  快捷通交易对账数据
-- -----------------------------------------------------
DROP TABLE IF EXISTS `parana_kjtpay_trans`;
CREATE TABLE IF NOT EXISTS `parana_kjtpay_trans` (
  `id`                          BIGINT        NOT NULL   AUTO_INCREMENT COMMENT '自增主键',
  `outer_no`                    VARCHAR(64)   NULL       COMMENT '商户订单号',
  `orig_outer_no`               VARCHAR(64)   NULL       COMMENT '原商户订单号',
  `inner_no`                    VARCHAR(64)   NULL       COMMENT '交易订单号',
  `type`                        VARCHAR(64)   NULL       COMMENT '交易类型',
  `amount`                      VARCHAR(64)   NULL       COMMENT '交易下单时间,支付时间',
  `rate`                        VARCHAR(32)   NULL       COMMENT '费率',
  `rate_fee`                    VARCHAR(32)   NULL       COMMENT '手续费',
  `status`                      VARCHAR(16)   NULL       COMMENT '状态',
  `order_at`                   DATETIME      NULL       COMMENT '交易下单时间',
  `paid_at`                   DATETIME      NULL       COMMENT '支付时间',
  `created_at`                  DATETIME      NULL       COMMENT '创建时间',
  `updated_at`                  DATETIME      NULL       COMMENT '修改时间',
  PRIMARY KEY (`id`)
);
CREATE INDEX idx_ekt_outer_no ON parana_kjtpay_trans (outer_no);
CREATE INDEX idx_ekt_inner_no ON parana_kjtpay_trans (inner_no);




