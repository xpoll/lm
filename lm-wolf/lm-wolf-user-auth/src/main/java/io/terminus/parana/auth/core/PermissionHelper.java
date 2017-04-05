package io.terminus.parana.auth.core;

import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Multimap;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.BaseUser;
import io.terminus.common.utils.Splitters;
import io.terminus.pampas.engine.ThreadVars;
import io.terminus.parana.auth.model.Acl;
import io.terminus.parana.auth.model.CategoryResource;
import io.terminus.parana.auth.model.DefaultRole;
import io.terminus.parana.auth.model.ExtendedRole;
import io.terminus.parana.auth.model.PermissionData;
import io.terminus.parana.auth.model.Request;
import io.terminus.parana.auth.model.RequestResource;
import io.terminus.parana.auth.model.Resource;
import io.terminus.parana.auth.model.TreeNode;
import io.terminus.parana.auth.role.CustomRoleLoader;
import io.terminus.parana.auth.role.CustomRoleLoaderRegistry;
import io.terminus.parana.auth.util.ExtendedRoleUtil;
import io.terminus.parana.common.utils.Iters;
import io.terminus.parana.common.utils.RespHelper;
import io.terminus.parana.user.auth.CustomRole;
import io.terminus.parana.user.auth.UserRoleLoader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import javax.annotation.Nullable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PermissionHelper {
   private static final Logger log = LoggerFactory.getLogger(PermissionHelper.class);
   private final UserRoleLoader userRoleLoader;
   private final CustomRoleLoaderRegistry customRoleLoaderRegistry;

   public PermissionHelper(UserRoleLoader userRoleLoader, CustomRoleLoaderRegistry customRoleLoaderRegistry) {
      this.userRoleLoader = userRoleLoader;
      this.customRoleLoaderRegistry = customRoleLoaderRegistry;
   }

   public PermissionData getPermissions(Acl acl, BaseUser user) {
      return this.getPermissions(acl, user, false);
   }

   public PermissionData getPermissions(Acl acl, BaseUser user, boolean noApis) {
      try {
         Set<Request> allRequests = new TreeSet();
         allRequests.addAll(this.getRequestsInAllDefaultRoles(acl, noApis));
         allRequests.addAll(this.getRequestsInAllResources(acl, noApis));
         Set<Request> requests = new TreeSet();
         Set<String> resources = new TreeSet(String.CASE_INSENSITIVE_ORDER);
         requests.addAll(this.getRequestsInDefaultRole(acl, "GLOBAL", noApis));
         resources.addAll(this.getResInDefaultRole(acl, "GLOBAL"));
         if(user == null) {
            return this.perm(requests, allRequests, resources);
         } else {
            requests.addAll(this.getRequestsInDefaultRole(acl, "LOGIN", noApis));
            resources.addAll(this.getResInDefaultRole(acl, "LOGIN"));
            List<String> roles = (List)RespHelper.orServEx(this.userRoleLoader.hardLoadRoles(user.getId()));
            Multimap<String, ExtendedRole> groupedRoles = ExtendedRoleUtil.groupUpRoles(roles);

            for(String baseRole : groupedRoles.keySet()) {
               DefaultRole defaultRole = this.getDefaultRole(acl, baseRole);
               if(defaultRole != null) {
                  Collection<ExtendedRole> er = groupedRoles.get(baseRole);
                  boolean isOwner = false;

                  for(ExtendedRole e : er) {
                     if("OWNER".equalsIgnoreCase(e.getLevel())) {
                        isOwner = true;
                        break;
                     }
                  }

                  if(isOwner) {
                     requests.addAll(this.getRequestsInDefaultRole(acl, defaultRole, noApis));
                     resources.addAll(this.getResInDefaultRole(defaultRole));
                  } else {
                     for(ExtendedRole e : er) {
                        if("SUB".equalsIgnoreCase(e.getLevel()) && e.getLevelContext() != null && !e.getLevelContext().isEmpty()) {
                           Set<String> rs = this.loadResources(acl, baseRole, e.getLevelContext());

                           for(String r : rs) {
                              for(RequestResource rr : this.getReqRes(acl, r)) {
                                 for(Request request : Iters.nullToEmpty(rr.getRequests())) {
                                    if(this.shouldAdded(noApis, request)) {
                                       requests.add(request);
                                    }
                                 }
                              }
                           }

                           resources.addAll(rs);
                        }
                     }
                  }
               }
            }

            return this.perm(requests, allRequests, resources);
         }
      } catch (Exception var24) {
         Throwables.propagateIfInstanceOf(var24, JsonResponseException.class);
         log.error("get permissions of user failed, cause:{}", Throwables.getStackTraceAsString(var24));
         throw new JsonResponseException("auth.permission.find.fail");
      }
   }

   private PermissionData perm(Set requests, Set allRequests, Set resources) {
      PermissionData perm = new PermissionData();
      perm.setRequests(new ArrayList(requests));
      perm.setAllRequests(new ArrayList(allRequests));
      perm.setResources(new ArrayList(resources));
      return perm;
   }

   private Set loadResources(Acl acl, String baseRole, List customRoleIds) {
      String appKey = ThreadVars.getAppKey();
      if(Strings.isNullOrEmpty(appKey)) {
         log.error("invalid appKey, null or empty");
         throw new RuntimeException("invalid appKey");
      } else {
         CustomRoleLoader loader = this.customRoleLoaderRegistry.getLoader(appKey, baseRole);
         if(loader == null) {
            log.warn("customRoleLoader for app={} and baseRole={} not supplied, can not load resources", appKey, baseRole);
            return Collections.emptySet();
         } else {
            Set<String> result = new TreeSet(String.CASE_INSENSITIVE_ORDER);

            for(CustomRole customRole : loader.load(customRoleIds)) {
               if(appKey.equalsIgnoreCase(customRole.getAppKey()) && customRole.isActive()) {
                  Map<String, TreeNode> tree = (Map)acl.getTrees().get(baseRole);
                  if(tree == null) {
                     log.warn("tree not found for baseRole={}", baseRole);
                  } else if(customRole.getAllow() != null && !customRole.getAllow().isEmpty()) {
                     Set<String> selected = new HashSet(customRole.getAllow());
                     Set<String> thisRoleResources = this.findResourcesInTree(tree, selected);
                     result.addAll(thisRoleResources);
                  }
               }
            }

            return result;
         }
      }
   }

   private Set findResourcesInTree(Map tree, Set selected) {
      if(tree == null) {
         return Collections.emptySet();
      } else {
         Set<String> result = new HashSet();

         for(String key : tree.keySet()) {
            TreeNode node = (TreeNode)tree.get(key);
            if(selected.contains(key) && node.getResources() != null) {
               result.addAll(node.getResources());
            }

            result.addAll(this.findResourcesInTree(node.getChildren(), selected));
         }

         return result;
      }
   }

   private Set getRequestsInAllResources(Acl acl, boolean noApis) {
      if(acl != null && acl.getResources() != null && !acl.getResources().isEmpty()) {
         Set<Request> result = new TreeSet();

         for(Resource resource : acl.getResources().values()) {
            if(resource instanceof CategoryResource) {
               CategoryResource cr = (CategoryResource)resource;

               for(RequestResource rr : cr.values()) {
                  for(Request request : Iters.nullToEmpty(rr.getRequests())) {
                     if(this.shouldAdded(noApis, request)) {
                        result.add(request);
                     }
                  }
               }
            } else if(resource instanceof RequestResource) {
               RequestResource rr = (RequestResource)resource;

               for(Request request : Iters.nullToEmpty(rr.getRequests())) {
                  if(this.shouldAdded(noApis, request)) {
                     result.add(request);
                  }
               }
            }
         }

         return result;
      } else {
         return Collections.emptySet();
      }
   }

   private Set getRequestsInAllDefaultRoles(Acl acl, boolean noApis) {
      if(acl != null && acl.getDefaults() != null && !acl.getDefaults().isEmpty()) {
         Set<Request> result = new TreeSet();

         for(String role : acl.getDefaults().keySet()) {
            result.addAll(this.getRequestsInDefaultRole(acl, role, noApis));
         }

         return result;
      } else {
         return Collections.emptySet();
      }
   }

   private Set getRequestsInDefaultRole(Acl acl, String role, boolean noApis) {
      DefaultRole defaultRole = this.getDefaultRole(acl, role);
      return defaultRole == null?Collections.emptySet():this.getRequestsInDefaultRole(acl, defaultRole, noApis);
   }

   private Set getResInDefaultRole(Acl acl, String role) {
      DefaultRole defaultRole = this.getDefaultRole(acl, role);
      return defaultRole == null?Collections.emptySet():this.getResInDefaultRole(defaultRole);
   }

   private Set getResInDefaultRole(DefaultRole defaultRole) {
      return (Set)(defaultRole.getResources() == null?Collections.emptySet():new TreeSet(defaultRole.getResources()));
   }

   @Nullable
   private DefaultRole getDefaultRole(Acl acl, String role) {
      return acl != null && acl.getDefaults() != null?(DefaultRole)acl.getDefaults().get(role):null;
   }

   private Set getRequestsInDefaultRole(Acl acl, DefaultRole defaultRole, boolean noApis) {
      Set<Request> result = new TreeSet();

      for(Request request : Iters.nullToEmpty(defaultRole.getRequests())) {
         if(this.shouldAdded(noApis, request)) {
            result.add(request);
         }
      }

      for(String r : Iters.nullToEmpty(defaultRole.getResources())) {
         for(RequestResource rr : this.getReqRes(acl, r)) {
            for(Request request : Iters.nullToEmpty(rr.getRequests())) {
               if(this.shouldAdded(noApis, request)) {
                  result.add(request);
               }
            }
         }
      }

      return result;
   }

   private Collection getReqRes(Acl acl, String r) {
      if(acl.getResources() != null && !acl.getResources().isEmpty()) {
         Collection<RequestResource> result = new ArrayList();
         List<String> parts = Splitters.SLASH.splitToList(r);
         if(parts.size() == 1) {
            Resource resource = (Resource)acl.getResources().get(parts.get(0));
            if(resource instanceof RequestResource) {
               RequestResource rr = (RequestResource)resource;
               result.add(rr);
            } else {
               log.warn("no group of key, resource must be RequestResource, key={}", r);
            }
         } else if(parts.size() == 2) {
            Resource resource = (Resource)acl.getResources().get(parts.get(0));
            if(resource instanceof CategoryResource) {
               CategoryResource cr = (CategoryResource)resource;
               result.addAll(cr.values());
            } else {
               log.warn("has group of key, resource must be CategoryResource, key={}", r);
            }
         } else {
            log.warn("key invalid, key={}", r);
         }

         return result;
      } else {
         return Collections.emptySet();
      }
   }

   private boolean shouldAdded(boolean noApis, Request request) {
      return !noApis || !this.isApi(request);
   }

   private boolean isApi(Request request) {
      String get = request.getGet();
      return Strings.isNullOrEmpty(get)?true:get.startsWith("/api");
   }
}
