package io.terminus.parana.pay.impl.manager;

import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.lib.pay.utils.NumberSwitch;
import io.terminus.parana.pay.common.ThirdPartyFeeAble;
import io.terminus.parana.pay.dto.ThirdPartyFeeDto;
import io.terminus.parana.pay.model.WechatpayTrans;
import io.terminus.parana.pay.service.PayReadService;
import java.util.Collection;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class WechatTransManager implements ThirdPartyFeeAble {
   private static final Logger log = LoggerFactory.getLogger(WechatTransManager.class);
   @Autowired
   private PayReadService payReadService;

   public ThirdPartyFeeDto getThirdPartyFee(String channel, String paymentCode) {
      ThirdPartyFeeDto thirdPartyFeeDto = new ThirdPartyFeeDto();
      return this.getWechatpayFee(paymentCode, channel, thirdPartyFeeDto);
   }

   public List getThirdPartyFeeRefunds(String paymentCode) {
      Response<List<WechatpayTrans>> transesRes = this.payReadService.findWechatpayTransByTransactionId(paymentCode);
      if(!transesRes.isSuccess()) {
         throw new ServiceException(transesRes.getError());
      } else if(Arguments.isNullOrEmpty((Collection)transesRes.getResult())) {
         return null;
      } else {
         List<ThirdPartyFeeDto> dtos = Lists.newArrayList();

         for(WechatpayTrans trans : (List)transesRes.getResult()) {
            String status = trans.getRefundStatus();
            if(Arguments.notEmpty(status) && Arguments.equalWith(status, "SUCCES")) {
               ThirdPartyFeeDto dto = new ThirdPartyFeeDto();
               dto.setFee(trans.refundFeeToFen());
               dtos.add(dto);
            }
         }

         return dtos;
      }
   }

   public String getInnerChannel() {
      return "wechatpay";
   }

   private ThirdPartyFeeDto getWechatpayFee(String paymentCode, String channel, ThirdPartyFeeDto thirdPartyFeeDto) {
      WechatpayTrans webTrans = this.getWechatpayTrans(paymentCode);
      thirdPartyFeeDto.setChannel(channel);
      thirdPartyFeeDto.setFee(NumberSwitch.stringToFen(webTrans.getTotalFee()));
      thirdPartyFeeDto.setThirdPartyFee(NumberSwitch.roundUp(webTrans.getPoundageFee()));
      thirdPartyFeeDto.setThirdPartyRate(NumberSwitch.percentToString(webTrans.getRate().substring(0, webTrans.getRate().length() - 1)));
      return thirdPartyFeeDto;
   }

   private WechatpayTrans getWechatpayTrans(String paymentCode) {
      Response<List<WechatpayTrans>> transesRes = this.payReadService.findWechatpayTransByTransactionId(paymentCode);
      if(!transesRes.isSuccess()) {
         throw new ServiceException(transesRes.getError());
      } else {
         Preconditions.checkState(!Arguments.isNullOrEmpty((Collection)transesRes.getResult()), "wechat.trans.not.found");
         if(((List)transesRes.getResult()).size() == 1) {
            return (WechatpayTrans)((List)transesRes.getResult()).get(0);
         } else {
            WechatpayTrans payTrans = null;

            for(WechatpayTrans trans : (List)transesRes.getResult()) {
               String status = trans.getTradeStatus();
               if(Arguments.notEmpty(status) && Arguments.equalWith(status, "SUCCESS")) {
                  payTrans = trans;
               }
            }

            if(payTrans != null) {
               Preconditions.checkState(!Strings.isNullOrEmpty(payTrans.getRate()), "wechat.commission.rate.null");
               Preconditions.checkState(!Strings.isNullOrEmpty(payTrans.getPoundageFee()), "wechat.commission.null");
               return payTrans;
            } else {
               throw new IllegalStateException("settlement.third.commission.not.match");
            }
         }
      }
   }

   public boolean match(String channel) {
      return !Strings.isNullOrEmpty(channel)?(channel.contains("wechatpay")?Boolean.TRUE.booleanValue():Boolean.FALSE.booleanValue()):Boolean.FALSE.booleanValue();
   }
}
