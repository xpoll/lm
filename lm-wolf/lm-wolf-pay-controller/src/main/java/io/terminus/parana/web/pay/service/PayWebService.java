package io.terminus.parana.web.pay.service;

import io.terminus.common.model.Response;
import io.terminus.lib.pay.channel.alipay.dto.RedirectInfo;
import io.terminus.parana.pay.enums.PayChannelBusinessType;
import java.util.List;

public interface PayWebService {
   Response refundRequest(Long var1, Integer var2, String var3, Integer var4, String var5, PayChannelBusinessType var6);

   RedirectInfo payRequest(List var1, Integer var2, String var3, Integer var4, String var5, Long var6);

   RedirectInfo payRequest(List var1, Integer var2, String var3, Integer var4, String var5, Long var6, String var7);
}
