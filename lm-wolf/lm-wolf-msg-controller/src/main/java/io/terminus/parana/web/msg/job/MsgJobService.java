package io.terminus.parana.web.msg.job;

public interface MsgJobService {
   Long batchSendMessages(Integer var1);

   Long closeMessages(Integer var1);
}
