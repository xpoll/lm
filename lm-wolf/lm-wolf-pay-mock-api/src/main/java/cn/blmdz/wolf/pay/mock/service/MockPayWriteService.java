package cn.blmdz.wolf.pay.mock.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.pay.mock.model.MockKjtpayTrans;
import cn.blmdz.wolf.pay.mock.model.MockUnionPayTrans;
import cn.blmdz.wolf.pay.mock.model.MockWechatpayTrans;
import cn.blmdz.wolf.pay.mock.model.MockedAlipayTrans;

public interface MockPayWriteService {
   Response createMockAlipayTrans(MockedAlipayTrans var1);

   Response createMockWechatPayTrans(MockWechatpayTrans var1, Integer var2);

   Response createMockUnionpayTrans(MockUnionPayTrans var1);

   Response createMockKjtpayTrans(MockKjtpayTrans var1);
}
