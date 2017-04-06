package cn.blmdz.wolf.pay.mock.service;

import java.util.Date;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.pay.mock.model.MockedAlipayTrans;

public interface MockedAlipayTransService {
   Response create(MockedAlipayTrans var1);

   Response getByTradeNo(String var1);

   Response getByMerchantOutOrderNo(String var1);

   Response findBy(MockedAlipayTrans var1, Date var2, Date var3, Integer var4, Integer var5);

   Response getAllByTradeNo(String var1);
}
