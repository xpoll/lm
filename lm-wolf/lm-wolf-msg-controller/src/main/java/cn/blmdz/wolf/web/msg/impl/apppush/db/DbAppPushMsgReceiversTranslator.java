package cn.blmdz.wolf.web.msg.impl.apppush.db;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.wolf.common.util.ExUtil;
import cn.blmdz.wolf.msg.dto.AppPushReceivers;
import cn.blmdz.wolf.user.model.UserDevice;
import cn.blmdz.wolf.user.service.DeviceReadService;
import cn.blmdz.wolf.web.msg.MsgReceiversTranslator;
import cn.blmdz.wolf.web.msg.impl.common.db.DbMsgReceiversTranslatorBase;

@Component
public class DbAppPushMsgReceiversTranslator extends DbMsgReceiversTranslatorBase implements MsgReceiversTranslator {
   private static final Logger log = LoggerFactory.getLogger(DbAppPushMsgReceiversTranslator.class);
   private final DeviceReadService deviceReadService;

   @Autowired
   public DbAppPushMsgReceiversTranslator(DeviceReadService deviceReadService) {
      this.deviceReadService = deviceReadService;
   }

   public String translateReceivers(String toes) {
      try {
         if(toes.contains("[") && !toes.contains("\"")) {
            List<Long> userIds = (List)JsonMapper.JSON_NON_EMPTY_MAPPER.fromJson(toes, JsonMapper.JSON_NON_EMPTY_MAPPER.createCollectionType(List.class, new Class[]{Long.class}));
            if(userIds != null) {
               AppPushReceivers receivers = new AppPushReceivers();

               for(Long userId : userIds) {
                  Response<List<UserDevice>> response = this.deviceReadService.findByUserId(userId);
                  if(response.isSuccess() && response.getResult() != null) {
                     for(UserDevice device : response.getResult()) {
                        if(device.getDeviceType().equals("android")) {
                           receivers.getAndroid().add(device.getDeviceToken());
                        } else if(device.getDeviceType().equals("ios")) {
                           receivers.getIos().add(device.getDeviceToken());
                        } else if(device.getDeviceType().equals("wp")) {
                           receivers.getWp().add(device.getDeviceToken());
                        }
                     }
                  }
               }

               toes = JsonMapper.JSON_NON_EMPTY_MAPPER.toJson(receivers);
            }
         }

         return toes;
      } catch (Exception var9) {
         throw ExUtil.logJsonEx(var9, "DbAppPushMsgReceiversTranslator", new Object[]{toes});
      }
   }
}
