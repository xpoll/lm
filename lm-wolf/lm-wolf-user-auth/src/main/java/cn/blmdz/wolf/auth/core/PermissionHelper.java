package cn.blmdz.wolf.auth.core;

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

import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Sets;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.util.Splitters;
import cn.blmdz.wolf.auth.model.Acl;
import cn.blmdz.wolf.auth.model.CategoryResource;
import cn.blmdz.wolf.auth.model.DefaultRole;
import cn.blmdz.wolf.auth.model.PermissionData;
import cn.blmdz.wolf.auth.model.Request;
import cn.blmdz.wolf.auth.model.RequestResource;
import cn.blmdz.wolf.auth.model.Resource;
import cn.blmdz.wolf.auth.model.TreeNode;
import cn.blmdz.wolf.common.utils.Iters;
import cn.blmdz.wolf.common.utils.RespHelper;
import cn.blmdz.wolf.user.auth.Role;
import cn.blmdz.wolf.user.auth.RoleContent;
import cn.blmdz.wolf.user.auth.UserRoleLoader;

public class PermissionHelper {
	private static final Logger log = LoggerFactory.getLogger(PermissionHelper.class);
	private final UserRoleLoader userRoleLoader;

	public PermissionHelper(UserRoleLoader userRoleLoader) {
		this.userRoleLoader = userRoleLoader;
	}

	public PermissionData getPermissions(Acl acl, BaseUser user) {
		return getPermissions(acl, user, false);
	}

	public PermissionData getPermissions(Acl acl, BaseUser user, boolean noApis) {
		try {
			Set allRequests = new TreeSet();
			allRequests.addAll(getRequestsInAllDefaultRoles(acl, noApis));
			allRequests.addAll(getRequestsInAllResources(acl, noApis));

			Set requests = new TreeSet();
			Set resources = new TreeSet(String.CASE_INSENSITIVE_ORDER);
			requests.addAll(getRequestsInDefaultRole(acl, "GLOBAL", noApis));
			resources.addAll(getResInDefaultRole(acl, "GLOBAL"));

			if (user == null) {
				return perm(requests, allRequests, resources);
			}

			requests.addAll(getRequestsInDefaultRole(acl, "LOGIN", noApis));
			resources.addAll(getResInDefaultRole(acl, "LOGIN"));

			Long realUserId = user.getId();

			RoleContent roleContent = (RoleContent) RespHelper.orServEx(this.userRoleLoader.hardLoadRoles(realUserId));
			for (Role staticRole : roleContent.getRoles()) {
				DefaultRole defaultRole = getDefaultRole(acl, staticRole.getBase());
				if (defaultRole == null) {
					continue;
				}
				requests.addAll(getRequestsInDefaultRole(acl, defaultRole, noApis));
				resources.addAll(getResInDefaultRole(defaultRole));
			}
			for (Role dynamicRole : roleContent.getDynamicRoles()) {
				Set<String> rs = findResourcesInTree(acl, dynamicRole.getBase(), dynamicRole.getTreeNodeSelection());
				for (String r : rs) {
					Collection<RequestResource> rrs = getReqRes(acl, r);
					for (RequestResource rr : rrs) {
						for (Request request : Iters.nullToEmpty(rr.getRequests())) {
							if (shouldAdded(noApis, request)) {
								requests.add(request);
							}
						}
					}
				}
				resources.addAll(rs);
			}
			return perm(requests, allRequests, resources);
		} catch (Exception e) {
			Throwables.propagateIfInstanceOf(e, JsonResponseException.class);
			log.error("get permissions of user failed, cause:{}", Throwables.getStackTraceAsString(e));
			throw new JsonResponseException("auth.permission.find.fail");
		}
	}

	protected PermissionData perm(Set<Request> requests, Set<Request> allRequests, Set<String> resources) {
		PermissionData perm = new PermissionData();
		perm.setRequests(new ArrayList(requests));
		perm.setAllRequests(new ArrayList(allRequests));
		perm.setResources(new ArrayList(resources));
		return perm;
	}

	protected Set<String> findResourcesInTree(Acl acl, String baseRole, List<String> treeNodeSelection) {
		Map tree = (Map) acl.getTrees().get(baseRole);
		if (tree == null) {
			log.warn("tree not found for baseRole={}", baseRole);
			return Collections.emptySet();
		}
		return findResourcesInTree(tree, Sets.newHashSet(treeNodeSelection));
	}

	protected Set<String> findResourcesInTree(Map<String, TreeNode> tree, Set<String> selected) {
		if (tree == null) {
			return Collections.emptySet();
		}
		Set result = new HashSet();
		for (String key : tree.keySet()) {
			TreeNode node = (TreeNode) tree.get(key);
			if ((selected.contains(key)) && (node.getResources() != null)) {
				result.addAll(node.getResources());
			}

			result.addAll(findResourcesInTree(node.getChildren(), selected));
		}
		return result;
	}

	protected Set<Request> getRequestsInAllResources(Acl acl, boolean noApis) {
		if ((acl == null) || (acl.getResources() == null) || (acl.getResources().isEmpty())) {
			return Collections.emptySet();
		}
		Set result = new TreeSet();
		for (Resource resource : acl.getResources().values()) {
			if (resource instanceof CategoryResource) {
				CategoryResource cr = (CategoryResource) resource;
				for (RequestResource rr : cr.values()) {
					for (Request request : Iters.nullToEmpty(rr.getRequests())) {
						if (shouldAdded(noApis, request))
							result.add(request);
					}
				}
			} else if (resource instanceof RequestResource) {
				RequestResource rr = (RequestResource) resource;
				for (Request request : Iters.nullToEmpty(rr.getRequests())) {
					if (shouldAdded(noApis, request)) {
						result.add(request);
					}
				}
			}
		}
		return result;
	}

	protected Set<Request> getRequestsInAllDefaultRoles(Acl acl, boolean noApis) {
		if ((acl == null) || (acl.getDefaults() == null) || (acl.getDefaults().isEmpty())) {
			return Collections.emptySet();
		}
		Set result = new TreeSet();
		for (String role : acl.getDefaults().keySet()) {
			result.addAll(getRequestsInDefaultRole(acl, role, noApis));
		}
		return result;
	}

	protected Set<Request> getRequestsInDefaultRole(Acl acl, String role, boolean noApis) {
		DefaultRole defaultRole = getDefaultRole(acl, role);
		if (defaultRole == null) {
			return Collections.emptySet();
		}
		return getRequestsInDefaultRole(acl, defaultRole, noApis);
	}

	protected Set<String> getResInDefaultRole(Acl acl, String role) {
		DefaultRole defaultRole = getDefaultRole(acl, role);
		if (defaultRole == null) {
			return Collections.emptySet();
		}
		return getResInDefaultRole(defaultRole);
	}

	protected Set<String> getResInDefaultRole(DefaultRole defaultRole) {
		if (defaultRole.getResources() == null) {
			return Collections.emptySet();
		}
		return new TreeSet(defaultRole.getResources());
	}

	@Nullable
	protected DefaultRole getDefaultRole(Acl acl, String role) {
		if ((acl == null) || (acl.getDefaults() == null)) {
			return null;
		}
		return (DefaultRole) acl.getDefaults().get(role);
	}

	protected Set<Request> getRequestsInDefaultRole(Acl acl, DefaultRole defaultRole, boolean noApis) {
		Set<Request> result = new TreeSet<Request>();
		for (Request request : Iters.<Request>nullToEmpty(defaultRole.getRequests())) {
			if (shouldAdded(noApis, request)) {
				result.add(request);
			}
		}
		for (String r : Iters.nullToEmpty(defaultRole.getResources())) {
			Collection<RequestResource> rrs = getReqRes(acl, r);
			for (RequestResource rr : rrs) {
				for (Request request : Iters.nullToEmpty(rr.getRequests())) {
					if (shouldAdded(noApis, request)) {
						result.add(request);
					}
				}
			}
		}
		return result;
	}

	protected Collection<RequestResource> getReqRes(Acl acl, String r) {
		if ((acl.getResources() == null) || (acl.getResources().isEmpty())) {
			return Collections.emptySet();
		}
		Collection result = new ArrayList();
		List parts = Splitters.SLASH.splitToList(r);
		if (parts.size() == 1) {
			Resource resource = (Resource) acl.getResources().get(parts.get(0));
			if (resource instanceof RequestResource) {
				RequestResource rr = (RequestResource) resource;
				result.add(rr);
			} else {
				log.warn("no group of key, resource must be RequestResource, key={}", r);
			}
		} else if (parts.size() == 2) {
			Resource resource = (Resource) acl.getResources().get(parts.get(0));
			if (resource instanceof CategoryResource) {
				CategoryResource cr = (CategoryResource) resource;
				result.addAll(cr.values());
			} else {
				log.warn("has group of key, resource must be CategoryResource, key={}", r);
			}
		} else {
			log.warn("key invalid, key={}", r);
		}
		return result;
	}

	protected boolean shouldAdded(boolean noApis, Request request) {
		return (!noApis) || (!isApi(request));
	}

	private boolean isApi(Request request) {
		String get = request.getGet();
		if (Strings.isNullOrEmpty(get)) {
			return true;
		}

		return get.startsWith("/api");
	}
}