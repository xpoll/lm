package cn.blmdz.wolf.pay.impl.gateway;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.annotation.PostConstruct;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

import com.google.common.base.Preconditions;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.wolf.pay.common.PayAbility;
import cn.blmdz.wolf.pay.common.ThirdPartyFeeAble;
import cn.blmdz.wolf.pay.dto.ThirdPartyFeeDto;

@Component
public class PayGateway {
   private static final Logger log = LoggerFactory.getLogger(PayGateway.class);
   @Autowired
   private ApplicationContext applicationContext;
   private Map<String, PayAbility> payAbilities;

   @PostConstruct
   public void init() {
      this.payAbilities = this.applicationContext.getBeansOfType(PayAbility.class);
   }

   public ThirdPartyFeeDto getThirdPartyFee(String channel, String paymentCode) {
      Preconditions.checkNotNull(channel, "channel null error");
      PayAbility payAbility = this.getPayAbility(channel);
      if(payAbility instanceof ThirdPartyFeeAble) {
         return ((ThirdPartyFeeAble)payAbility).getThirdPartyFee(channel, paymentCode);
      } else {
         throw new ServiceException("this channel not implement thirdpartyfeeable");
      }
   }

   public List getThirdPartyFeeRefunds(String channel, String paymentCode) {
      Preconditions.checkNotNull(channel, "channel null error");
      PayAbility payAbility = this.getPayAbility(channel);
      if(payAbility instanceof ThirdPartyFeeAble) {
         return ((ThirdPartyFeeAble)payAbility).getThirdPartyFeeRefunds(paymentCode);
      } else {
         throw new ServiceException("this channel not implement thirdpartyfeeable");
      }
   }

   public String findInnerChannelByChannel(String channel) {
      Preconditions.checkNotNull(channel, "channel null error");
      PayAbility payAbility = this.getPayAbility(channel);
      if(payAbility instanceof ThirdPartyFeeAble) {
         return ((ThirdPartyFeeAble)payAbility).getInnerChannel();
      } else {
         throw new ServiceException("this channel not implement thirdpartyfeeable");
      }
   }

   private PayAbility getPayAbility(String channel) {
      for(Entry<String, PayAbility> entry : this.payAbilities.entrySet()) {
         if(((PayAbility)entry.getValue()).match(channel)) {
            return (PayAbility)entry.getValue();
         }
      }

      throw new ServiceException("Pay channel[" + channel + "] not match any instance");
   }
}
