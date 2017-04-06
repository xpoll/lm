package cn.blmdz.wolf.msg.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.msg.model.ReceiverGroup;

public interface ReceiverGroupWriteService {
   Response createReceiverGroup(ReceiverGroup var1);

   Response updateReceiverGroup(ReceiverGroup var1);

   Response deleteReceiverGroup(Long var1);
}
