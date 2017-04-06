package cn.blmdz.wolf.web.msg;

import java.util.List;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.wolf.msg.model.Message;

public interface MsgTemplateApplication {
   List applyTemplate(Message var1) throws JsonResponseException;
}
