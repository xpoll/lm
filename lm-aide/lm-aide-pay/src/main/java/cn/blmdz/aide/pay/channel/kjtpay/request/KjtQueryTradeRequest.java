package cn.blmdz.aide.pay.channel.kjtpay.request;

import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.kevinsawicki.http.HttpRequest;
import com.google.common.base.Preconditions;
import com.google.common.base.Splitter;
import com.google.common.base.Splitter.MapSplitter;
import com.google.common.base.Strings;

import cn.blmdz.home.common.util.Arguments;

public class KjtQueryTradeRequest extends Request {
   private static final Logger log = LoggerFactory.getLogger(KjtQueryTradeRequest.class);
   public static final Splitter SQUARE = Splitter.on("^").omitEmptyStrings().trimResults();

   private KjtQueryTradeRequest(KjtToken kjtToken) {
      super(kjtToken);
      this.params.put("service", "query_trade");
      this.params.put("memo", "");
      this.params.put("identity_type", "1");
   }

   public static KjtQueryTradeRequest build(KjtToken kjtToken) {
      return new KjtQueryTradeRequest(kjtToken);
   }

   public KjtQueryTradeRequest memo(String memo) {
      if(Arguments.notNull(memo)) {
         this.params.put("memo", memo);
      }

      return this;
   }

   public KjtQueryTradeRequest setTradeNo(String tradeNo) {
      Preconditions.checkArgument(Arguments.notNull(tradeNo), "kjt.pay.outer.trade.no.empty");
      this.params.put("outer_trade_no", tradeNo);
      return this;
   }

   public KjtQueryTradeRequest setSign(String sign) {
      Preconditions.checkArgument(Arguments.notNull(sign), "kjt.pay.sign.empty");
      this.params.put("sign", sign);
      return this;
   }

   public String query() {
      String url = super.url();
      log.debug("kjtpay query trade url: {}", url);
      String body = HttpRequest.get(url).connectTimeout(10000).readTimeout(10000).body();
      log.debug("kjtpay query trade result: {}", body);
      MapSplitter splitter = Splitter.on("&").withKeyValueSeparator("=");
      Map<String, String> map = splitter.split(body);
      if(!Objects.equals(map.get("is_success"), "T")) {
         log.info("error_message is: {}", map.get("error_message"));
         return (String)map.get("error_message");
      } else {
         String tradeList = (String)map.get("tradeList");
         if(Strings.isNullOrEmpty(tradeList)) {
            log.info("tradeList is null, cause:{}", map.get("memo"));
            return (String)map.get("memo");
         } else {
            List<String> resultList = SQUARE.splitToList(tradeList);
            return (String)resultList.get(6);
         }
      }
   }
}
