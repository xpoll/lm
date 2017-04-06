insert into `parana_order_flows` (`id`, `name`, `desc`, `created_at`, `updated_at`) values (1, "在线支付流程", "b2b2c在线支付", now(), now());


insert into `parana_order_node_instances` (`id`, `parent_id`, `fid`, `names`, `desc`, `created_at`, `updated_at`) values 
-- 店铺订单状态
(1, -1, 1, '{"buyer":"等待买家付款","seller":"等待买家付款","admin":"等待买家付款"}', "店铺订单在线支付", now(), now()),
(2, 1, 1, '{"buyer":"等待卖家发货","seller":"待发货","admin":"等待卖家发货"}', "店铺订单在线支付", now(), now()),
(3, 2, 1, '{"buyer":"卖家已发货","seller":"已发货","admin":"卖家已发货"}', "店铺订单在线支付", now(), now()),
(4, 3, 1, '{"buyer":"交易完成","seller":"交易完成","admin":"交易完成"}', "店铺订单在线支付", now(), now()),
(5, 1, 1, '{"buyer":"订单已取消","seller":"买家取消订单","admin":"买家取消订单"}', "店铺订单在线支付", now(), now()),
(6, 1, 1, '{"buyer":"卖家取消订单","seller":"订单已取消","admin":"卖家取消订单"}', "店铺订单在线支付", now(), now()),
(7, 2, 1, '{"buyer":"已申请退款","seller":"买家申请退款","admin":"买家申请退款"}', "店铺订单在线支付", now(), now()),
(8, 7, 1, '{"buyer":"卖家同意退款","seller":"同意退款","admin":"卖家同意退款"}', "店铺订单在线支付", now(), now()),
(9, 7, 1, '{"buyer":"卖家拒绝退款","seller":"拒绝退款","admin":"卖家拒绝退款"}', "店铺订单在线支付", now(), now()),
(10, 3, 1, '{"buyer":"已申请退货","seller":"买家申请退货","admin":"买家申请退货"}', "店铺订单在线支付", now(), now()),
(11, 10, 1, '{"buyer":"卖家同意退货","seller":"同意退货","admin":"卖家同意退货"}', "店铺订单在线支付", now(), now()),
(12, 10, 1, '{"buyer":"卖家拒绝退货","seller":"拒绝退货","admin":"卖家拒绝退货"}', "店铺订单在线支付", now(), now()),
(13, 11, 1, '{"buyer":"已发退还货物","seller":"买家已发退还货物","admin":"买家已发退还货物"}', "店铺订单在线支付", now(), now()),
(14, 13, 1, '{"buyer":"卖家已确认收货,退款成功","seller":"确认收货,退款成功","admin":"卖家已确认收货,退款成功"}', "店铺订单在线支付", now(), now()),
(39, 2, 1, '{\"buyer\":\"卖家同意退款\",\"seller\":\"同意退款\",\"admin\":\"卖家同意退款\"}', '店铺订单在线支付,由skuOrder退款触发', now(), now()),

-- sku 订单状态
(15, -1, 1, '{"buyer":"等待买家付款","seller":"等待买家付款","admin":"等待买家付款"}', "sku订单在线支付", now(), now()),
(16, 15, 1, '{"buyer":"等待卖家发货","seller":"待发货","admin":"等待卖家发货"}', "sku订单在线支付", now(), now()),
(17, 16, 1, '{"buyer":"卖家已发货","seller":"已发货","admin":"卖家已发货"}', "sku订单在线支付", now(), now()),
(18, 17, 1, '{"buyer":"交易完成","seller":"交易完成","admin":"交易完成"}', "sku订单在线支付", now(), now()),
(19, 15, 1, '{"buyer":"订单已取消","seller":"买家取消订单","admin":"买家取消订单"}', "sku订单在线支付", now(), now()),
(20, 15, 1, '{"buyer":"卖家取消订单","seller":"订单已取消","admin":"卖家取消订单"}', "sku订单在线支付", now(), now()),
(21, 16, 1, '{"buyer":"已申请退款","seller":"买家申请退款","admin":"买家申请退款"}', "sku订单在线支付", now(), now()),
(22, 21, 1, '{"buyer":"卖家同意退款","seller":"同意退款","admin":"卖家同意退款"}', "sku订单在线支付", now(), now()),
(23, 21, 1, '{"buyer":"卖家拒绝退款","seller":"拒绝退款","admin":"卖家拒绝退款"}', "sku订单在线支付", now(), now()),
(24, 17, 1, '{"buyer":"已申请退货","seller":"买家申请退货","admin":"买家申请退货"}', "sku订单在线支付", now(), now()),
(25, 24, 1, '{"buyer":"卖家同意退货","seller":"同意退货","admin":"卖家同意退货"}', "sku订单在线支付", now(), now()),
(26, 24, 1, '{"buyer":"卖家拒绝退货","seller":"拒绝退货","admin":"卖家拒绝退货"}', "sku订单在线支付", now(), now()),
(27, 25, 1, '{"buyer":"已发退还货物","seller":"买家已发退还货物","admin":"买家已发退还货物"}', "sku订单在线支付", now(), now()),
(28, 27, 1, '{"buyer":"卖家已确认收货,退款成功","seller":"确认收货,退款成功","admin":"卖家已确认收货,退款成功"}', "sku订单在线支付", now(), now()),

-- sku退款单状态
(29, -1, 1, '{"buyer":"已申请退款","seller":"买家申请退款","admin":"买家申请退款"}', "sku退款单在线支付", now(), now()),
(30, 29, 1, '{"buyer":"卖家同意退款","seller":"同意退款","admin":"卖家同意退款"}', "sku退款单在线支付", now(), now()),
(31, 29, 1, '{"buyer":"卖家拒绝退款","seller":"拒绝退款","admin":"卖家拒绝退款"}', "sku退款单在线支付", now(), now()),
(32, -1, 1, '{"buyer":"已申请退货","seller":"买家申请退货","admin":"买家申请退货"}', "sku退款单在线支付", now(), now()),
(33, 32, 1, '{"buyer":"卖家同意退货","seller":"同意退货","admin":"卖家同意退货"}', "sku退款单在线支付", now(), now()),
(34, 32, 1, '{"buyer":"卖家拒绝退货","seller":"拒绝退货","admin":"卖家拒绝退货"}', "sku退款单在线支付", now(), now()),
(35, 33, 1, '{"buyer":"已发退还货物","seller":"买家已发退还货物","admin":"买家已发退还货物"}', "sku退款单在线支付", now(), now()),
(36, 35, 1, '{"buyer":"卖家已确认收货,退款成功","seller":"确认收货,退款成功","admin":"卖家已确认收货,退款成功"}', "sku退款单在线支付", now(), now()),
(37, 29, 1, '{"buyer":"已撤销退款","seller":"买家撤销退款","admin":"买家撤销退款"}', "sku退款单在线支付", now(), now()),
(38, 32, 1, '{"buyer":"已撤销退货","seller":"买家撤销退货","admin":"买家撤销退货"}', "sku退款单在线支付", now(), now());


insert into `parana_order_action_instances` (`id`, `node_instance_id`, `action`, `display`, `type`, `belong_user_types`, `expire_minutes`, `name`, `desc`, `created_at`, `updated_at`) values
-- 店铺订单动作
(1, 1, "orderPrePayAction", true, 1, "[1]", null, "支付", "店铺订单支付", now(), now()),
(2, 2, "orderDeliveAction", true, 1, "[2]", null, "发货", "店铺订单发货", now(), now()),
(3, 3, "orderConfirmAction", true, 1, "[1]", null, "确认收货", "店铺订单确认收货", now(), now()),
(4, 1, "orderBuyerCancelAction", true, 1, "[1]", null, "买家取消订单", "店铺订单买家取消订单", now(), now()),
(5, 1, "orderSellerCancelAction", true, 1, "[2]", null, "卖家取消订单", "店铺订单卖家取消订单", now(), now()),
(6, 2, "orderApplyRefundAction", true, 1, "[1]", null, "申请退款", "店铺订单申请退款", now(), now()),
(7, 7, "orderAgreeRefundAction", true, 1, "[2]", null, "同意退款", "店铺订单同意退款", now(), now()),
(8, 7, "orderRefuseRefundAction", true, 1, "[2]", null, "拒绝退款", "店铺订单拒绝退款", now(), now()),
(9, 3, "orderApplyReturnsAction", true, 1, "[1]", null, "申请退货", "店铺订单申请退货", now(), now()),
(10, 9, "orderAgreeReturnsAction", true, 1, "[2]", null, "同意退货", "店铺订单同意退货", now(), now()),
(11, 9, "orderRefuseReturnsAction", true, 1, "[2]", null, "拒绝退货", "店铺订单拒绝退货", now(), now()),
(12, 10, "orderBuyerDeliveReturnsAction", true, 1, "[1]", null, "买家发退还货物", "店铺订单买家发出退还货物", now(), now()),
(13, 12, "orderSellerConfirmReturnsAction", true, 1, "[2]", null, "卖家确认收到退还货物", "店铺订单卖家确认收到退款货物", now(), now()),
(14, 7, "orderUndoRefundAction", true, 1, "[1]", null, "撤销退款申请", "店铺订单撤销退款申请", now(), now()),
(15, 10, "orderUndoReturnsAfterApplyAction", true, 1, "[1]", null, "撤销退货申请", "店铺订单撤销退货申请", now(), now()),
(16, 11, "orderUndoReturnsAfterAgreeAction", true, 1, "[1]", null, "撤销退货申请", "店铺订单撤销退货申请", now(), now()),
(28, 1, "orderPaidAction", false, 1, "[1]", null, "支付回调动作", "店铺订单支付回调动作，由第三方支付平台触发", now(), now()),

-- sku订单动作
(17, 16, "skuOrderApplyRefundAction", true, 1, "[1]", null, "申请退款", "sku订单申请退款", now(), now()),
(20, 17, "skuOrderApplyReturnsAction", true, 1, "[1]", null, "申请退货", "sku订单申请退货", now(), now()),

-- sku 退款单动作
(18, 29, "skuOrderPreRefundAction", true, 1, "[2]", null, "同意退款,生成退款地址", "sku退款单同意退款", now(), now()),
(19, 29, "skuOrderRefuseRefundAction", true, 1, "[2]", null, "拒绝退款", "sku退款单拒绝退款", now(), now()),
(21, 32, "skuOrderAgreeReturnsAction", true, 1, "[2]", null, "同意退货", "sku退款单同意退货", now(), now()),
(22, 32, "skuOrderRefuseReturnsAction", true, 1, "[2]", null, "拒绝退货", "sku退款单拒绝退货", now(), now()),
(23, 33, "skuOrderBuyerDeliveReturnsAction", true, 1, "[1]", null, "买家发退还货物", "sku退款单买家发出退还货物", now(), now()),
(24, 35, "skuOrderPreReturnsAction", true, 1, "[2]", null, "卖家确认收到退还货物,发起退款", "sku退款单卖家确认收到退款货物", now(), now()),
(25, 29, "skuOrderUndoRefundAction", true, 1, "[1]", null, "撤销退款申请", "sku退款单撤销退款申请", now(), now()),
(26, 32, "skuOrderUndoReturnsAfterApplyAction", true, 1, "[1]", null, "撤销退货申请", "sku退款单撤销退货申请", now(), now()),
(27, 33, "skuOrderUndoReturnsAfterAgreeAction", true, 1, "[1]", null, "撤销退货申请", "sku退款单撤销退货申请", now(), now()),
(29, 29, "skuOrderRefundCallBackAction",  false, 1, "[2]", null, "退款回调", "sku退款单退款回调", now(), now()),
(30, 35, "skuOrderReturnsCallBackAction", false, 1, "[2]", null, "退货退款回调", "sku退款单退货退款回调", now(), now()),
(31, 23, "skuOrderApplyRefundAction", 1, 1, "[1]", NULL, "申请退款, 退款被拒绝后的再次申请退款", "sku订单申请退款", now(), now());



insert into `parana_order_transfer_rules` (`start_node_instance_id`, `end_node_instance_id`, `created_at`, `updated_at`) values
  -- 总订单流转
  (1, 2, now(), now()),
  (2, 3, now(), now()),
  (3, 4, now(), now()),
  (1, 5, now(), now()),
  (1, 6, now(), now()),
  (2, 39, now(), now()),
  (3, 41, now(), now()),

  -- 子订单流转
  (15, 16, now(), now()),
  (16, 17, now(), now()),
  (17, 18, now(), now()),
  (15, 19, now(), now()),
  (15, 20, now(), now()),
  (17, 42, now(), now()),
  (16, 43, now(), now()),

  -- sku退款单流转
  (29, 30, now(), now()),
  (29, 31, now(), now()),
  (29, 37, now(), now()),
  (32, 33, now(), now()),
  (33, 35, now(), now()),
  (35, 36, now(), now()),
  (32, 34, now(), now()),
  (32, 38, now(), now()),
  (33, 40, now(), now());