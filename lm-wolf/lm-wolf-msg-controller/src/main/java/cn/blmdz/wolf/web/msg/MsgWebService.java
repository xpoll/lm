package cn.blmdz.wolf.web.msg;

import java.util.Map;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.wolf.msg.model.Message;

public interface MsgWebService {
   String send(String var1, String var2, Map var3, String var4) throws JsonResponseException;

   String send(String var1, String var2, String var3, String var4) throws JsonResponseException;

   String send(Message var1) throws JsonResponseException;
}
