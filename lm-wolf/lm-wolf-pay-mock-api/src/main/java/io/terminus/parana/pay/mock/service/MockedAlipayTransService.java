package io.terminus.parana.pay.mock.service;

import io.terminus.common.model.Response;
import io.terminus.parana.pay.mock.model.MockedAlipayTrans;
import java.util.Date;

public interface MockedAlipayTransService {
   Response create(MockedAlipayTrans var1);

   Response getByTradeNo(String var1);

   Response getByMerchantOutOrderNo(String var1);

   Response findBy(MockedAlipayTrans var1, Date var2, Date var3, Integer var4, Integer var5);

   Response getAllByTradeNo(String var1);
}
