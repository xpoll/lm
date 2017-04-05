package io.terminus.parana.web.msg;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.parana.msg.model.Message;
import java.util.List;

public interface MsgTemplateApplication {
   List applyTemplate(Message var1) throws JsonResponseException;
}
