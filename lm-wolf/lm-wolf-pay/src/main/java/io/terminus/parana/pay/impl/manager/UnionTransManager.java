package io.terminus.parana.pay.impl.manager;

import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;
import io.terminus.common.utils.Arguments;
import io.terminus.parana.pay.common.ThirdPartyFeeAble;
import io.terminus.parana.pay.dto.ThirdPartyFeeDto;
import io.terminus.parana.pay.impl.dao.UnionPayTransDao;
import io.terminus.parana.pay.model.UnionPayTrans;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class UnionTransManager implements ThirdPartyFeeAble {
   private static final Logger log = LoggerFactory.getLogger(UnionTransManager.class);
   @Autowired
   private UnionPayTransDao unionPayTransDao;

   public ThirdPartyFeeDto getThirdPartyFee(String channel, String paymentCode) {
      ThirdPartyFeeDto thirdPartyFeeDto = new ThirdPartyFeeDto();
      thirdPartyFeeDto.setChannel(channel);
      UnionPayTrans trans = this.unionPayTransDao.findForwardByQueryId(paymentCode);
      Preconditions.checkState(!Arguments.isNull(trans), "union.pay.trans.not.exist");
      thirdPartyFeeDto.setFee(trans.getTxnAmt());
      thirdPartyFeeDto.setThirdPartyFee(trans.getThirdPartyFee());
      thirdPartyFeeDto.setThirdPartyRate("暂无");
      return thirdPartyFeeDto;
   }

   public List getThirdPartyFeeRefunds(String paymentCode) {
      List<UnionPayTrans> transes = this.unionPayTransDao.findReverseByQueryId(paymentCode);
      if(Arguments.isNullOrEmpty(transes)) {
         return null;
      } else {
         List<ThirdPartyFeeDto> dtos = Lists.newArrayList();

         for(UnionPayTrans trans : transes) {
            ThirdPartyFeeDto dto = new ThirdPartyFeeDto();
            dto.setFee(trans.getTxnAmt());
            dtos.add(dto);
         }

         return dtos;
      }
   }

   public String getInnerChannel() {
      return "unionpay";
   }

   public boolean match(String channel) {
      return !Strings.isNullOrEmpty(channel)?(channel.contains("unionpay")?Boolean.TRUE.booleanValue():Boolean.FALSE.booleanValue()):Boolean.FALSE.booleanValue();
   }
}
