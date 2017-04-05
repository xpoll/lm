package io.terminus.parana.msg.service;

import io.terminus.common.model.Response;
import io.terminus.parana.msg.model.ReceiverGroup;

public interface ReceiverGroupWriteService {
   Response createReceiverGroup(ReceiverGroup var1);

   Response updateReceiverGroup(ReceiverGroup var1);

   Response deleteReceiverGroup(Long var1);
}
