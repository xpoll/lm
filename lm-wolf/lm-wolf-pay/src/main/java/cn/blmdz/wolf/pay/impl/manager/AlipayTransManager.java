package cn.blmdz.wolf.pay.impl.manager;

import java.util.Collection;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;

import cn.blmdz.aide.pay.utils.NumberSwitch;
import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.wolf.pay.common.ThirdPartyFeeAble;
import cn.blmdz.wolf.pay.dto.ThirdPartyFeeDto;
import cn.blmdz.wolf.pay.model.AlipayTrans;
import cn.blmdz.wolf.pay.service.PayReadService;

@Component
public class AlipayTransManager implements ThirdPartyFeeAble {
   private static final Logger log = LoggerFactory.getLogger(AlipayTransManager.class);
   @Autowired
   private PayReadService payReadService;

   public ThirdPartyFeeDto getThirdPartyFee(String channel, String paymentCode) {
      ThirdPartyFeeDto thirdPartyFeeDto = new ThirdPartyFeeDto();
      return this.getAlipayFee(paymentCode, channel, thirdPartyFeeDto);
   }

   public List getThirdPartyFeeRefunds(String paymentCode) {
      return this.getAlipayTransRefund(paymentCode);
   }

   public String getInnerChannel() {
      return "alipay";
   }

   private ThirdPartyFeeDto getAlipayFee(String paymentCode, String channel, ThirdPartyFeeDto thirdPartyFeeDto) {
      AlipayTrans alipayTrans = this.getAlipayTrans(paymentCode);
      if(Arguments.isNull(alipayTrans)) {
         return null;
      } else {
         thirdPartyFeeDto.setChannel(channel);
         thirdPartyFeeDto.setFee(NumberSwitch.stringToFen(alipayTrans.getTotalFee()));
         thirdPartyFeeDto.setThirdPartyRate(alipayTrans.getRate());
         thirdPartyFeeDto.setThirdPartyFee(alipayTrans.outcomeToFen());
         return thirdPartyFeeDto;
      }
   }

   private AlipayTrans getAlipayTrans(String paymentCode) {
      Response<List<AlipayTrans>> transesRes = this.payReadService.findAlipayTransByTradeNo(paymentCode);
      if(!transesRes.isSuccess()) {
         throw new ServiceException(transesRes.getError());
      } else if(Arguments.isNull(transesRes.getResult())) {
         return null;
      } else if(((List)transesRes.getResult()).size() == 1) {
         return (AlipayTrans)((List)transesRes.getResult()).get(0);
      } else {
         AlipayTrans payTrans = null;

         for(AlipayTrans trans : transesRes.getResult()) {
            String subTransCodeMsg = trans.getSubTransCodeMsg();
            if(Arguments.notEmpty(subTransCodeMsg) && Arguments.equalWith(subTransCodeMsg, "收费")) {
               payTrans = trans;
               break;
            }

            String transCodeMsg = trans.getTransCodeMsg();
            if(Arguments.notEmpty(transCodeMsg) && Arguments.equalWith(transCodeMsg, "在线支付")) {
               payTrans = trans;
            }
         }

         if(payTrans != null) {
            Preconditions.checkState(!Strings.isNullOrEmpty(payTrans.getTotalFee()), "alipay.total.fee.null");
            Preconditions.checkState(!Strings.isNullOrEmpty(payTrans.getOutcome()), "alipay.commission.null");
            if(Strings.isNullOrEmpty(payTrans.getRate())) {
               payTrans.setRate(NumberSwitch.getAlipayRate(payTrans.getOutcome(), payTrans.getTotalFee()));
            }

            return payTrans;
         } else {
            throw new IllegalStateException("settlement.third.commission.not.match");
         }
      }
   }

   private List getAlipayTransRefund(String paymentCode) {
      Response<List<AlipayTrans>> transesRes = this.payReadService.findAlipayTransByTradeNo(paymentCode);
      if(!transesRes.isSuccess()) {
         throw new ServiceException(transesRes.getError());
      } else if(Arguments.isNullOrEmpty((Collection)transesRes.getResult())) {
         return null;
      } else {
         List<ThirdPartyFeeDto> dtos = Lists.newArrayList();

         for(AlipayTrans trans : transesRes.getResult()) {
            String subTransCodeMsg = trans.getSubTransCodeMsg();
            if(Arguments.notEmpty(subTransCodeMsg) && Arguments.equalWith(subTransCodeMsg, "交易退款")) {
               ThirdPartyFeeDto dto = new ThirdPartyFeeDto();
               dto.setFee(trans.outcomeToFen());
               dtos.add(dto);
            }
         }

         return dtos;
      }
   }

   public boolean match(String channel) {
      return !Strings.isNullOrEmpty(channel)?(channel.contains("alipay")?Boolean.TRUE.booleanValue():Boolean.FALSE.booleanValue()):Boolean.FALSE.booleanValue();
   }
}
