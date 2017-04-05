-- --------------------------------------------------------
-- Table `mocked_alipay_trans`  支付宝交易对账数据（模拟数据，仅在测试环境使用）
-- ------------------------------------------------------------------
DROP TABLE IF EXISTS `parana_mock_alipay_trans`;

CREATE TABLE IF NOT EXISTS `parana_mock_alipay_trans` (
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
  `created_at`                  DATETIME      NULL       COMMENT '创建时间',
  `updated_at`                  DATETIME      NULL       COMMENT '修改时间',
  PRIMARY KEY (`id`)
);



-- 微信支付账务明细
CREATE TABLE `parana_mock_wechatpay_trans` (
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

DROP TABLE IF EXISTS `parana_mock_unionpay_trans`;

CREATE TABLE IF NOT EXISTS `parana_mock_unionpay_trans` (
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
CREATE INDEX idx_pmut_query_id ON parana_mock_unionpay_trans (query_id);


-- -----------------------------------------------------
-- Table `parana_kjtpay_trans`  快捷通交易对账数据
-- -----------------------------------------------------
DROP TABLE IF EXISTS `parana_mock_kjtpay_trans`;
CREATE TABLE IF NOT EXISTS `parana_mock_kjtpay_trans` (
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
CREATE INDEX idx_pmkt_outer_no ON parana_mock_kjtpay_trans (outer_no);
CREATE INDEX idx_pmkt_inner_no ON parana_mock_kjtpay_trans (inner_no);