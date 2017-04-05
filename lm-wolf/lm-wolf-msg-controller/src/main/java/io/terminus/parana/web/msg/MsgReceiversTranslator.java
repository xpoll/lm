package io.terminus.parana.web.msg;

import io.terminus.common.exception.JsonResponseException;

public interface MsgReceiversTranslator {
   String translateReceivers(String var1) throws JsonResponseException;
}
