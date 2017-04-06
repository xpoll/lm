package cn.blmdz.wolf.auth.web.controller;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.wolf.auth.core.AclLoader;
import cn.blmdz.wolf.auth.core.PermissionHelper;
import cn.blmdz.wolf.auth.model.Acl;
import cn.blmdz.wolf.auth.model.CompiledTree;
import cn.blmdz.wolf.auth.model.ParanaThreadVars;
import cn.blmdz.wolf.auth.model.PermissionData;
import cn.blmdz.wolf.auth.model.TreeNode;
import cn.blmdz.wolf.common.utils.Iters;

@RestController
@RequestMapping({"/api/auth"})
public class AuthApis {
   private static final Logger log = LoggerFactory.getLogger(AuthApis.class);
   private final AclLoader aclLoader;
   private final PermissionHelper permissionHelper;

   @Autowired
   public AuthApis(AclLoader aclLoader, PermissionHelper permissionHelper) {
      this.aclLoader = aclLoader;
      this.permissionHelper = permissionHelper;
   }

   @RequestMapping(
      value = {"/tree"},
      method = {RequestMethod.GET}
   )
   public CompiledTree getTree(@RequestParam String role) {
      try {
         Acl acl = this.aclLoader.getAcl(ParanaThreadVars.getApp());
         Map<String, TreeNode> tree = (Map)Iters.nullToEmpty(acl.getTrees()).get(role);
         if(tree == null) {
            log.warn("auth tree not found, role={}", role);
            throw new JsonResponseException("auth.tree.not.found");
         } else {
            CompiledTree ct = new CompiledTree();
            ct.setAppKey(ParanaThreadVars.getAppKey());
            ct.setBaseRole(role);
            ct.setChildren(this.compileTree(tree));
            return ct;
         }
      } catch (Exception var5) {
         Throwables.propagateIfInstanceOf(var5, JsonResponseException.class);
         log.error("get auth tree failed, role={}, cause:{}", role, Throwables.getStackTraceAsString(var5));
         throw new JsonResponseException("auth.tree.find.fail");
      }
   }

   private List compileTree(Map<String, TreeNode> tree) {
      if(tree == null) {
         return new ArrayList();
      } else {
         List<CompiledTree.Node> result = new ArrayList();

         for(String key : tree.keySet()) {
            TreeNode raw = (TreeNode)tree.get(key);
            CompiledTree.Node n = new CompiledTree.Node();
            result.add(n);
            n.setKey(key);
            n.setName(raw.getName());
            n.setDescription(raw.getDescription());
            n.setSelected(Boolean.FALSE);
            n.setChildren(this.compileTree(raw.getChildren()));
         }

         return result;
      }
   }

   @RequestMapping(
      value = {"/permissions"},
      method = {RequestMethod.GET}
   )
   public PermissionData getPermissions(@RequestParam(
   required = false,
   defaultValue = "false"
) boolean noApis) {
      try {
         Acl acl = this.aclLoader.getAcl(ParanaThreadVars.getApp());
         BaseUser user = UserUtil.getCurrentUser();
         PermissionData perm = this.permissionHelper.getPermissions(acl, user, noApis);
         perm.setAllRequests((List)null);
         return perm;
      } catch (Exception var5) {
         Throwables.propagateIfInstanceOf(var5, JsonResponseException.class);
         log.error("get permissions of user failed, cause:{}", Throwables.getStackTraceAsString(var5));
         throw new JsonResponseException("auth.permission.find.fail");
      }
   }
}
