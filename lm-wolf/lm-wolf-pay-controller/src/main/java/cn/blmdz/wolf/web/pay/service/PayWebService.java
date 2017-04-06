package cn.blmdz.wolf.web.pay.service;

import java.util.List;

import cn.blmdz.aide.pay.channel.alipay.dto.RedirectInfo;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.pay.enums.PayChannelBusinessType;

public interface PayWebService {
   Response refundRequest(Long var1, Integer var2, String var3, Integer var4, String var5, PayChannelBusinessType var6);

   RedirectInfo payRequest(List var1, Integer var2, String var3, Integer var4, String var5, Long var6);

   RedirectInfo payRequest(List var1, Integer var2, String var3, Integer var4, String var5, Long var6, String var7);
}
