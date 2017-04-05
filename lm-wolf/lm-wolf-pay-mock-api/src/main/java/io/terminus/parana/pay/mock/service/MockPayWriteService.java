package io.terminus.parana.pay.mock.service;

import io.terminus.common.model.Response;
import io.terminus.parana.pay.mock.model.MockKjtpayTrans;
import io.terminus.parana.pay.mock.model.MockUnionPayTrans;
import io.terminus.parana.pay.mock.model.MockWechatpayTrans;
import io.terminus.parana.pay.mock.model.MockedAlipayTrans;

public interface MockPayWriteService {
   Response createMockAlipayTrans(MockedAlipayTrans var1);

   Response createMockWechatPayTrans(MockWechatpayTrans var1, Integer var2);

   Response createMockUnionpayTrans(MockUnionPayTrans var1);

   Response createMockKjtpayTrans(MockKjtpayTrans var1);
}
