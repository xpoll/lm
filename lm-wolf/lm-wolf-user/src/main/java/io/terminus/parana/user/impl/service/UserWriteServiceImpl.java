package io.terminus.parana.user.impl.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.parana.common.utils.EncryptUtil;
import io.terminus.parana.user.impl.dao.UserDao;
import io.terminus.parana.user.model.User;
import io.terminus.parana.user.service.UserWriteService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

@Service
public class UserWriteServiceImpl implements UserWriteService {
   private static final Logger log = LoggerFactory.getLogger(UserWriteServiceImpl.class);
   private final UserDao userDao;

   @Autowired
   public UserWriteServiceImpl(UserDao userDao) {
      this.userDao = userDao;
   }

   public Response create(User user) {
      try {
         if(StringUtils.hasText(user.getPassword())) {
            user.setPassword(EncryptUtil.encrypt(user.getPassword()));
         }

         this.userDao.create(user);
         return Response.ok(user.getId());
      } catch (DuplicateKeyException var3) {
         log.error("failed to create {}, cause:{}", user, Throwables.getStackTraceAsString(var3));
         return Response.fail("user.loginId.duplicate");
      } catch (Exception var4) {
         log.error("failed to create {}, cause:{}", user, Throwables.getStackTraceAsString(var4));
         return Response.fail("user.create.fail");
      }
   }

   public Response update(User user) {
      try {
         this.userDao.update(user);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var3) {
         log.error("failed to update {}, cause:{}", user, Throwables.getStackTraceAsString(var3));
         return Response.fail("user.update.fail");
      }
   }
}
