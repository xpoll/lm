package io.terminus.parana.web.pay.service;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Throwables;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.common.utils.BeanMapper;
import io.terminus.lib.pay.channel.alipay.dto.AlipaySettlementResponse;
import io.terminus.lib.pay.channel.alipay.dto.RedirectInfo;
import io.terminus.lib.pay.channel.alipay.dto.settlement.AlipaySettlementDto;
import io.terminus.lib.pay.channel.alipay.manager.AlipayManager;
import io.terminus.lib.pay.channel.alipay.request.AlipayToken;
import io.terminus.lib.pay.channel.alipay.request.PageQueryRequest;
import io.terminus.parana.config.ConfigCenter;
import io.terminus.parana.pay.model.AlipayTrans;
import io.terminus.parana.pay.service.PayWriteService;
import java.util.Date;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SyncThirdTransReadService {
   private static final Logger log = LoggerFactory.getLogger(SyncThirdTransReadService.class);
   @Autowired
   private ConfigCenter configCenter;
   @Autowired
   private PayWriteService payWriteService;
   private DateTimeFormatter STD_FMT = DateTimeFormat.forPattern("YYYY-MM-dd HH:mm:ss");

   public Response syncAlipayTrans(Date start, Date end, Integer pageNo, Integer pageSize) {
      Response<Integer> result = new Response();

      try {
         AlipayToken token = this.getAlipayToken();
         AlipayManager alipayManager = new AlipayManager();
         RedirectInfo info = alipayManager.syncThridTrans(start, end, pageNo, pageSize, token);
         if(!info.isSuccess()) {
            throw new ServiceException(info.getError());
         }

         log.debug("query url: {}", info.getResult());
         String body = HttpRequest.get(info.getResult()).connectTimeout(10000).readTimeout(10000).body();
         log.debug("return body content: {}", body);
         AlipaySettlementResponse resp = PageQueryRequest.queryResultToObject(body);
         if(resp == null || resp.getResult() == null || resp.getResult().getPaging() == null || resp.getResult().getPaging().getAccountLogList() == null) {
            result.setResult(Integer.valueOf(0));
            return result;
         }

         for(AlipaySettlementDto alipaySettlementDto : resp.getResult().getPaging().getAccountLogList()) {
            AlipayTrans trans = (AlipayTrans)BeanMapper.map(alipaySettlementDto, AlipayTrans.class);
            trans.setTradeAt(this.STD_FMT.parseDateTime(trans.getTransDate()).toDate());
            Response<Boolean> createRes = this.payWriteService.createAlipayTrans(trans);
            if(!createRes.isSuccess()) {
               log.error("create alipay trans: {} fail,error: {} ", trans, createRes.getError());
            }
         }

         result.setResult(Integer.valueOf(resp.getResult().getPaging().getAccountLogList().size()));
      } catch (ServiceException var15) {
         log.error("sync alipay trans fail start:{} end: {} ,error: {}", new Object[]{start, end, var15.getMessage()});
         result.setError(var15.getMessage());
      } catch (Exception var16) {
         log.error("sync alipay trans fail start:{} end: {} ,cause: {}", new Object[]{start, end, Throwables.getStackTraceAsString(var16)});
         result.setError("sync.alipay.trans.fail");
      }

      return result;
   }

   private AlipayToken getAlipayToken() {
      if(!this.configCenter.get("alipay.account").isPresent()) {
         throw new ServiceException("config.alipay.account.not.exist");
      } else if(!this.configCenter.get("alipay.gateway").isPresent()) {
         throw new ServiceException("config.alipay.gateway.not.exist");
      } else if(!this.configCenter.get("alipay.key").isPresent()) {
         throw new ServiceException("config.alipay.key.not.exist");
      } else if(!this.configCenter.get("alipay.pid").isPresent()) {
         throw new ServiceException("config.alipay.pid.not.exist");
      } else if(!this.configCenter.get("alipay.wap.gateway").isPresent()) {
         throw new ServiceException("config.alipay.wap.gateway.not.exist");
      } else {
         AlipayToken token = new AlipayToken();
         token.setAccount((String)this.configCenter.get("alipay.account").get());
         token.setGateway((String)this.configCenter.get("alipay.gateway").get());
         token.setKey((String)this.configCenter.get("alipay.key").get());
         token.setPid((String)this.configCenter.get("alipay.pid").get());
         token.setWapGateway((String)this.configCenter.get("alipay.wap.gateway").get());
         return token;
      }
   }
}
