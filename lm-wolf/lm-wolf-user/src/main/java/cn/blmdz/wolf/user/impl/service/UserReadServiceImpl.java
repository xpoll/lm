package cn.blmdz.wolf.user.impl.service;

import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import com.google.common.base.Throwables;
import com.google.common.collect.Maps;

import cn.blmdz.home.common.model.PageInfo;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.common.utils.EncryptUtil;
import cn.blmdz.wolf.user.impl.dao.UserDao;
import cn.blmdz.wolf.user.model.LoginType;
import cn.blmdz.wolf.user.model.User;
import cn.blmdz.wolf.user.service.UserReadService;

@Service
public class UserReadServiceImpl implements UserReadService<User> {
   private static final Logger log = LoggerFactory.getLogger(UserReadServiceImpl.class);
   private final UserDao userDao;

   @Autowired
   public UserReadServiceImpl(UserDao userDao) {
      this.userDao = userDao;
   }

   public Response findById(Long id) {
      try {
         User user = (User)this.userDao.findById(id);
         if(user == null) {
            log.error("user(id={}) is not found", id);
            return Response.fail("user.not.found");
         } else {
            return Response.ok(user);
         }
      } catch (Exception var3) {
         log.error("failed to find user(id={}), cause:{}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("user.find.fail");
      }
   }

   public Response findByIds(List ids) {
      try {
         return Response.ok(this.userDao.findByIds(ids));
      } catch (Exception var3) {
         log.error("find users by ids={} failed, cause:{}", ids, Throwables.getStackTraceAsString(var3));
         return Response.fail("user.find.fail");
      }
   }

   public Response findBy(String loginId, LoginType loginType) {
      try {
         User user;
         switch(loginType) {
         case NAME:
            user = this.userDao.findByName(loginId);
            break;
         case EMAIL:
            user = this.userDao.findByEmail(loginId);
            break;
         case MOBILE:
            user = this.userDao.findByMobile(loginId);
            break;
         default:
            return Response.fail("user.not.found");
         }

         if(user == null) {
            log.error("user(loginId={}, loginType={}) not found", loginId, loginType);
            return Response.fail("user.not.found");
         } else {
            return Response.ok(user);
         }
      } catch (Exception var4) {
         log.error("failed to find user(loginId={}, loginType={}), cause:{}", new Object[]{loginId, loginType, Throwables.getStackTraceAsString(var4)});
         return Response.fail("user.find.fail");
      }
   }

   public Response login(String loginId, String password, LoginType loginType) {
      Response<User> rUser = this.findBy(loginId, loginType);
      if(!rUser.isSuccess()) {
         return rUser;
      } else {
         User user = (User)rUser.getResult();
         if(user.getStatus().intValue() < 0) {
            log.error("user(loginId={}, loginType={})\'s status is {}, login forbidden ", new Object[]{loginId, loginType, user.getStatus()});
            switch(user.getStatus().intValue()) {
            case -3:
               return Response.fail("user.status.deleted");
            case -2:
               return Response.fail("user.status.frozen");
            case -1:
               return Response.fail("user.status.locked");
            default:
               return Response.fail("user.status.abnormal");
            }
         } else if(!EncryptUtil.match(password, user.getPassword())) {
            log.error("user(loginId={}, loginType={})\'s password mismatch, login failed", loginId, loginType);
            return Response.fail("user.password.mismatch");
         } else {
            return rUser;
         }
      }
   }

   public Response paging(Long id, String name, String email, String mobile, Integer status, Integer type, Integer pageNo, Integer pageSize) {
      Map<String, Object> paramas = Maps.newHashMap();

      try {
         if(id != null) {
            paramas.put("id", id);
         }

         if(status != null) {
            paramas.put("status", status);
         }

         if(type != null) {
            paramas.put("type", type);
         }

         if(StringUtils.hasText(name)) {
            paramas.put("name", name);
         }

         if(StringUtils.hasText(email)) {
            paramas.put("email", email);
         }

         if(StringUtils.hasText(mobile)) {
            paramas.put("mobile", mobile);
         }

         PageInfo page = PageInfo.of(pageNo, pageSize);
         Paging<User> userPage = this.userDao.paging(page.getOffset(), page.getLimit(), paramas);
         return Response.ok(userPage);
      } catch (Exception var12) {
         log.error("fail to page user by params:{}, cause:{}", paramas, Throwables.getStackTraceAsString(var12));
         return Response.fail("user.find.fail");
      }
   }
}
