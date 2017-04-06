package cn.blmdz.wolf.msg.service;

import java.util.Map;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.msg.model.Message;

public interface MessageWriteService {
   Response sendMessage(Integer var1, String var2, String var3, String var4, String var5);

   Response sendMessageViaTemplate(Integer var1, String var2, Map var3, String var4, String var5);

   Response createMessage(Message var1);

   Response updateMessage(Message var1);
}
