package io.terminus.parana.pay.mock.impl.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.parana.pay.mock.impl.dao.MockKjtpayTransDao;
import io.terminus.parana.pay.mock.impl.dao.MockUnionPayTransDao;
import io.terminus.parana.pay.mock.impl.dao.MockWechatpayTransDao;
import io.terminus.parana.pay.mock.impl.dao.MockedAlipayTransDao;
import io.terminus.parana.pay.mock.model.MockKjtpayTrans;
import io.terminus.parana.pay.mock.model.MockUnionPayTrans;
import io.terminus.parana.pay.mock.model.MockWechatpayTrans;
import io.terminus.parana.pay.mock.model.MockedAlipayTrans;
import io.terminus.parana.pay.mock.service.MockPayWriteService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MockPayWriteServiceImpl implements MockPayWriteService {
   private static final Logger log = LoggerFactory.getLogger(MockPayWriteServiceImpl.class);
   @Autowired
   private MockedAlipayTransDao alipayTransDao;
   @Autowired
   private MockUnionPayTransDao unionPayTransDao;
   @Autowired
   private MockKjtpayTransDao kjtpayTransDao;
   @Autowired
   private MockWechatpayTransDao wechatpayTransDao;

   public Response createMockAlipayTrans(MockedAlipayTrans trans) {
      Response<Boolean> result = new Response();

      try {
         this.alipayTransDao.createIsNotExist(trans);
         result.setResult(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("create alipay trans fail,cause: {}", Throwables.getStackTraceAsString(var4));
         result.setError("create.alipay.trans.fail");
      }

      return result;
   }

   public Response createMockWechatPayTrans(MockWechatpayTrans trans, Integer type) {
      Response<Boolean> result = new Response();

      try {
         MockWechatpayTrans trans1;
         if(type.equals(Integer.valueOf(1))) {
            trans1 = this.wechatpayTransDao.findByOutTradeNoAndTradeStatus(trans.getOutTradeNo(), "SUCCESS");
         } else {
            trans1 = this.wechatpayTransDao.findByOutRefundNoAndRefundStatus(trans.getOutRefundNo(), "SUCCESS");
         }

         if(Arguments.isNull(trans1)) {
            this.wechatpayTransDao.create(trans);
         }

         result.setResult(Boolean.TRUE);
      } catch (Exception var5) {
         log.error("create wechat pay trans fail,cause: {}", Throwables.getStackTraceAsString(var5));
         result.setError("create.wechat.pay.trans.fail");
      }

      return result;
   }

   public Response createMockUnionpayTrans(MockUnionPayTrans trans) {
      Response<Boolean> result = new Response();

      try {
         MockUnionPayTrans exist = this.unionPayTransDao.findByOrderId(trans.getOrderId());
         if(Arguments.isNull(exist)) {
            this.unionPayTransDao.create(trans);
         }

         result.setResult(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("create unionpay trans fail,cause: {}", Throwables.getStackTraceAsString(var4));
         result.setError("create.unionpay.trans.fail");
      }

      return result;
   }

   public Response createMockKjtpayTrans(MockKjtpayTrans trans) {
      Response<Boolean> result = new Response();

      try {
         if(Arguments.isNull(this.kjtpayTransDao.findByInnerNo(trans.getInnerNo()))) {
            this.kjtpayTransDao.create(trans);
         }

         result.setResult(Boolean.TRUE);
      } catch (Exception var4) {
         log.error("create kjtpay trans fail,cause: {}", Throwables.getStackTraceAsString(var4));
         result.setError("create.kjtpay.trans.fail");
      }

      return result;
   }
}
