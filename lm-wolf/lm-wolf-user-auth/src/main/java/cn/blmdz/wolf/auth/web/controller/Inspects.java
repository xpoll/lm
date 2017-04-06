package cn.blmdz.wolf.auth.web.controller;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.wolf.auth.core.AclLoader;
import cn.blmdz.wolf.auth.core.Authenticator;
import cn.blmdz.wolf.auth.model.Acl;
import cn.blmdz.wolf.auth.model.ParanaThreadVars;
import cn.blmdz.wolf.auth.model.Req;
import cn.blmdz.wolf.common.model.ParanaUser;

@RestController
@RequestMapping({"/api/auth/inspect"})
public class Inspects {
   private final AclLoader aclLoader;
   private final Authenticator authenticator;

   @Autowired
   public Inspects(AclLoader aclLoader, Authenticator authenticator) {
      this.aclLoader = aclLoader;
      this.authenticator = authenticator;
   }

   @RequestMapping(
      value = {"/acl"},
      method = {RequestMethod.GET}
   )
   public Acl getParsed() {
      return this.aclLoader.getAcl(ParanaThreadVars.getApp());
   }

   @RequestMapping(
      value = {"/check-req"},
      method = {RequestMethod.GET}
   )
   public boolean check(@RequestParam String path, @RequestParam String method) {
      return this.authenticator.ask((ParanaUser)UserUtil.getCurrentUser(), new Req(path, method, (Map)null));
   }

   @RequestMapping(
      value = {"/check-key"},
      method = {RequestMethod.GET}
   )
   public boolean check(@RequestParam String key) {
      return this.authenticator.ask((ParanaUser)UserUtil.getCurrentUser(), key);
   }
}
