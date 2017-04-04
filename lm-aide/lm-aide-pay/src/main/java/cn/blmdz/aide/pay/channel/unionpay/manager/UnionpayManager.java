package io.terminus.lib.pay.channel.unionpay.manager;

import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.lib.pay.channel.alipay.dto.RedirectInfo;
import io.terminus.lib.pay.channel.unionpay.request.UnionPayRequest;
import io.terminus.lib.pay.channel.unionpay.request.UnionQueryTradeRequest;
import io.terminus.lib.pay.channel.unionpay.request.UnionRefundRequest;
import io.terminus.lib.pay.channel.unionpay.request.UnionToken;
import io.terminus.lib.pay.enums.TradeType;
import io.terminus.lib.pay.exception.PayRequestParamException;
import io.terminus.lib.pay.utils.PayParamValid;
import java.util.Date;
import java.util.List;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class UnionpayManager {
   private final DateTimeFormatter DT = DateTimeFormat.forPattern("MMdd");

   public RedirectInfo pay(String channel, String tradeNo, String systemNo, Integer fee, String notifyUrl, String returnUrl, UnionToken unionToken) {
      RedirectInfo info = new RedirectInfo();
      this.checkParam(notifyUrl, returnUrl, (String)null, (String)null, unionToken, channel, Integer.valueOf(TradeType.PAY.value()));
      UnionPayRequest payRequest = UnionPayRequest.build(unionToken).backUrl(notifyUrl).frontUrl(returnUrl).bizType("000201").channelType("08").orderId(tradeNo).reqReserved(systemNo).txnAmt(fee);
      String json = payRequest.submitPayRequestForm();
      info.setIsRedirectNow(Boolean.FALSE);
      info.setChannel(channel);
      info.setResult(json);
      return info;
   }

   public RedirectInfo refund(Long refundTrackId, String tradeNo, String channel, String refundNo, String paymentCode, Integer refundAmount, String refundNotifyUrl, String reason, UnionToken unionToken) {
      RedirectInfo info = new RedirectInfo();
      PayParamValid.validRefundParam(refundTrackId, channel, tradeNo, refundNo, paymentCode, refundAmount);
      this.checkParam((String)null, (String)null, refundNotifyUrl, "2", unionToken, channel, Integer.valueOf(TradeType.REFUND.value()));
      UnionRefundRequest refundRequest = UnionRefundRequest.build(unionToken).backUrl(refundNotifyUrl).bizType("000201").channelType("08").orderId(refundNo).origQryId(paymentCode).txnAmt(refundAmount).reqReserved(Strings.isNullOrEmpty(reason)?"退款":reason);
      String params = refundRequest.url();
      info.setResult(params);
      info.setIsRedirectNow(Boolean.TRUE);
      info.setChannel(channel);
      return info;
   }

   public Response sysncThirdTrans(Date date, UnionToken unionToken) {
      Preconditions.checkArgument(Arguments.notNull(date), "sysc.unionpay.trans.context.billDate.is.null");
      String billDate = this.DT.print(new DateTime(date));
      UnionQueryTradeRequest queryTradeRequest = UnionQueryTradeRequest.build(unionToken).bizType("000000").settleDate(billDate).fileType("00");
      return queryTradeRequest.query();
   }

   private void checkParam(String notifyUrl, String returnUrl, String refundNotifyUrl, String refundType, UnionToken unionToken, String channel, Integer type) {
      if(Strings.isNullOrEmpty(unionToken.getMerId())) {
         throw new PayRequestParamException("pay.request.param.merid.invalid");
      } else {
         if(type.equals(Integer.valueOf(TradeType.PAY.value()))) {
            if(Strings.isNullOrEmpty(unionToken.getFrontTransUrl())) {
               throw new PayRequestParamException("pay.request.param.front.trans.url.invalid");
            }

            if(Strings.isNullOrEmpty(notifyUrl)) {
               throw new PayRequestParamException("pay.request.param.notify.url.invalid");
            }

            if(Strings.isNullOrEmpty(returnUrl)) {
               throw new PayRequestParamException("pay.request.param.return.url.invalid");
            }
         }

         if(type.equals(Integer.valueOf(TradeType.REFUND.value()))) {
            if(Strings.isNullOrEmpty(refundNotifyUrl)) {
               throw new PayRequestParamException("refund.request.param.notify.url.invalid");
            }

            if(Strings.isNullOrEmpty(refundType)) {
               throw new PayRequestParamException("refund.request.param.type.invalid");
            }

            if(Strings.isNullOrEmpty(unionToken.getBackTransUrl())) {
               throw new PayRequestParamException("pay.request.param.back.trans.url.invalid");
            }
         }

         if(type.equals(Integer.valueOf(TradeType.SYNC_SETTLEMENT.value())) && Strings.isNullOrEmpty(unionToken.getFileTransUrl())) {
            throw new PayRequestParamException("pay.request.param.file.trans.url.invalid");
         }
      }
   }

   public String getFee(String fee) {
      if(Arguments.isNull(fee)) {
         return "0";
      } else {
         int len = fee.length();
         int index = 0;
         List<String> list = Lists.newArrayList();

         for(int m = 1; m < 10; ++m) {
            list.add(String.valueOf(m));
         }

         char[] strs = fee.toCharArray();

         for(int i = 0; i < len; ++i) {
            if(list.contains(String.valueOf(strs[i]))) {
               index = i;
               break;
            }

            if(i == len - 1) {
               return "0";
            }
         }

         return fee.substring(index, len);
      }
   }
}
