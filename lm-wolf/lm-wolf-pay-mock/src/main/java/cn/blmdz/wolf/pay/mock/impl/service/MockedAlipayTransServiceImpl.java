package cn.blmdz.wolf.pay.mock.impl.service;

import java.util.Date;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Optional;
import com.google.common.base.Preconditions;
import com.google.common.base.Throwables;
import com.google.common.collect.Maps;

import cn.blmdz.home.common.model.PageInfo;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.wolf.pay.mock.impl.dao.MockedAlipayTransDao;
import cn.blmdz.wolf.pay.mock.model.MockedAlipayTrans;
import cn.blmdz.wolf.pay.mock.service.MockedAlipayTransService;

@Service
public class MockedAlipayTransServiceImpl implements MockedAlipayTransService {
   private static final Logger log = LoggerFactory.getLogger(MockedAlipayTransServiceImpl.class);
   @Autowired
   private MockedAlipayTransDao mockedAlipayTransDao;

   public Response create(MockedAlipayTrans trans) {
      Response<Long> res = new Response();

      try {
         Long id = this.mockedAlipayTransDao.createIsNotExist(trans);
         res.setResult(id);
      } catch (Exception var4) {
         log.error("fail to create mockedAlipayTrans with trans:{}, cause:{}", trans, Throwables.getStackTraceAsString(var4));
         res.setError("mocked.trans.create.fail");
      }

      return res;
   }

   public Response getByTradeNo(String tradeNo) {
      Response<MockedAlipayTrans> res = new Response();

      try {
         List<MockedAlipayTrans> mockedAlipayTranses = this.mockedAlipayTransDao.list(tradeNo);
         Optional<MockedAlipayTrans> payTrans = Optional.absent();

         for(MockedAlipayTrans trans : mockedAlipayTranses) {
            if(Arguments.notNull(trans.getSubTransCodeMsg()) && trans.getSubTransCodeMsg().startsWith("快速支付")) {
               payTrans = Optional.of(trans);
               break;
            }
         }

         Preconditions.checkState(payTrans.isPresent(), "mocked.pay.trans.not.found");
         res.setResult(payTrans.get());
         return res;
      } catch (IllegalStateException var7) {
         log.info("fail to get MockedAlipayTrans with transNo:{}, cause:{}", tradeNo, var7.getMessage());
         res.setError(var7.getMessage());
      } catch (Exception var8) {
         log.info("fail to get MockedAlipayTrans with transNo:{}, cause:{}", tradeNo, Throwables.getStackTraceAsString(var8));
         res.setError("mocked.trans.query.fail");
      }

      return res;
   }

   public Response getByMerchantOutOrderNo(String merchantOutOrderNo) {
      Response<MockedAlipayTrans> res = new Response();

      try {
         List<MockedAlipayTrans> mockedAlipayTranses = this.mockedAlipayTransDao.findByMerchantNo(merchantOutOrderNo);
         Optional<MockedAlipayTrans> payTrans = Optional.absent();

         for(MockedAlipayTrans trans : mockedAlipayTranses) {
            if(Arguments.notNull(trans.getSubTransCodeMsg()) && trans.getSubTransCodeMsg().startsWith("快速支付")) {
               payTrans = Optional.of(trans);
               break;
            }
         }

         Preconditions.checkState(payTrans.isPresent(), "mocked.pay.trans.not.found");
         res.setResult(payTrans.get());
         return res;
      } catch (IllegalStateException var7) {
         log.info("fail to get MockedAlipayTrans with merchantOutOrderNo:{}, error:{}", merchantOutOrderNo, var7.getMessage());
         res.setError(var7.getMessage());
      } catch (Exception var8) {
         log.info("fail to get MockedAlipayTrans with merchantOutOrderNo:{}, cause:{}", merchantOutOrderNo, Throwables.getStackTraceAsString(var8));
         res.setError("mocked.trans.query.fail");
      }

      return res;
   }

   public Response findBy(MockedAlipayTrans criteria, Date startAt, Date endAt, Integer pageNo, Integer size) {
      Response<Paging<MockedAlipayTrans>> res = new Response();

      try {
         Map<String, Object> params = Maps.newHashMap();
         params.put("criteria", criteria);
         params.put("createdStartAt", startAt);
         params.put("createdEndAt", endAt);
         PageInfo pageInfo = new PageInfo(pageNo, size);
         params.put("offset", pageInfo.getOffset());
         params.put("limit", pageInfo.getLimit());
         Paging<MockedAlipayTrans> paging = this.mockedAlipayTransDao.findBy(params);
         res.setResult(paging);
      } catch (Exception var10) {
         log.info("fail to get MockedAlipayTrans with startAt:{}, endAt{}, transNo:{}, merchantNo:{} cause:{}", new Object[]{startAt, endAt, criteria.getTradeNo(), criteria.getMerchantOutOrderNo(), Throwables.getStackTraceAsString(var10)});
         res.setError("mocked.trans.query.fail");
      }

      return res;
   }

   public Response getAllByTradeNo(String tradeNo) {
      Response<List<MockedAlipayTrans>> res = new Response();

      try {
         List<MockedAlipayTrans> mockedAlipayTranses = this.mockedAlipayTransDao.list(tradeNo);
         res.setResult(mockedAlipayTranses);
         return res;
      } catch (Exception var4) {
         log.info("fail to get MockedAlipayTrans with transNo:{}, cause:{}", tradeNo, Throwables.getStackTraceAsString(var4));
         res.setError("mocked.trans.query.fail");
         return res;
      }
   }
}
