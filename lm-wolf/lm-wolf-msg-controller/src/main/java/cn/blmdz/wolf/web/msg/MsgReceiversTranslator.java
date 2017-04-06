package cn.blmdz.wolf.web.msg;

import cn.blmdz.home.common.exception.JsonResponseException;

public interface MsgReceiversTranslator {
   String translateReceivers(String var1) throws JsonResponseException;
}
