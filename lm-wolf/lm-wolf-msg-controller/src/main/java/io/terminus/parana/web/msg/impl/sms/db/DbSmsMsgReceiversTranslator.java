package io.terminus.parana.web.msg.impl.sms.db;

import com.google.common.base.Strings;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.common.utils.JsonMapper;
import io.terminus.parana.common.util.ExUtil;
import io.terminus.parana.user.model.User;
import io.terminus.parana.user.service.UserReadService;
import io.terminus.parana.web.msg.MsgReceiversTranslator;
import io.terminus.parana.web.msg.impl.common.db.DbMsgReceiversTranslatorBase;
import java.util.ArrayList;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class DbSmsMsgReceiversTranslator extends DbMsgReceiversTranslatorBase implements MsgReceiversTranslator {
   private static final Logger log = LoggerFactory.getLogger(DbSmsMsgReceiversTranslator.class);
   private final UserReadService userReadService;

   @Autowired
   public DbSmsMsgReceiversTranslator(UserReadService userReadService) {
      this.userReadService = userReadService;
   }

   public String translateReceivers(String toes) {
      try {
         if(toes.contains("[") && !toes.contains("\"")) {
            List<Long> userIds = (List)JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(toes, JsonMapper.JSON_NON_EMPTY_MAPPER.createCollectionType(List.class, new Class[]{Long.class}));
            if(userIds != null) {
               List<String> smsList = new ArrayList();
               Response<List<User>> userResponse = this.userReadService.findByIds(userIds);
               if(!userResponse.isSuccess()) {
                  log.error("translateReceivers failed, toes={}, cause={}", toes, userResponse.getResult());
                  throw new JsonResponseException("find.user.by.ids.failed");
               }

               for(User user : (List)userResponse.getResult()) {
                  if(Strings.isNullOrEmpty(user.getMobile())) {
                     log.warn("translateReceivers missing user mobile for user={}", user);
                  } else {
                     smsList.add(user.getMobile());
                  }
               }

               if(smsList.isEmpty()) {
                  log.error("receivers list is empty, toes={}", toes);
                  throw new JsonResponseException("receiver.list.empty");
               }

               toes = JsonMapper.JSON_NON_EMPTY_MAPPER.toJson(smsList);
            }
         }

         return toes;
      } catch (Exception var7) {
         throw ExUtil.logJsonEx(var7, "DbSmsMsgReceiversTranslator", new Object[]{toes});
      }
   }
}
