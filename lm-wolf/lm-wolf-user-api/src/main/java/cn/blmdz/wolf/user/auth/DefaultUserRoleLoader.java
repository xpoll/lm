package cn.blmdz.wolf.user.auth;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.user.model.User;
import cn.blmdz.wolf.user.service.AdminUserService;
import cn.blmdz.wolf.user.service.UserReadService;

public class DefaultUserRoleLoader implements UserRoleLoader {
	private static final Logger log = LoggerFactory.getLogger(DefaultUserRoleLoader.class);
	private final UserReadService<User> userReadService;
	private final AdminUserService adminUserService;
	private final RoleProviderRegistry roleProviderRegistry;

	public DefaultUserRoleLoader(UserReadService<User> userReadService, AdminUserService adminUserService,
			RoleProviderRegistry roleProviderRegistry) {
		this.userReadService = userReadService;
		this.adminUserService = adminUserService;
		this.roleProviderRegistry = roleProviderRegistry;
	}

	public Response<RoleContent> hardLoadRoles(Long userId) {
		try {
			if (userId == null) {
				log.warn("hard load roles failed, userId=null");
				return Response.fail("user.id.empty");
			}
			Response findResp = this.userReadService.findById(userId);
			if (!findResp.isSuccess()) {
				log.warn("find user failed, userId={}, error={}", userId, findResp.getError());
				return Response.fail(findResp.getError());
			}
			User user = (User) findResp.getResult();
			if (user == null) {
				log.warn("hard load roles failed, user not found, id={}", userId);
				return Response.fail("user.not.found");
			}

			if (user.getType() == null) {
				log.warn("user has no type, userId={}, we treat is as empty permission", userId);
				return Response.ok(initRoles());
			}
			int userType = user.getType().intValue();

			RoleContent mutableRoles = initRoles();

			List<RoleProvider> roleProviders = this.roleProviderRegistry.getRoleProviders();
			if (!CollectionUtils.isEmpty(roleProviders)) {
				for (RoleProvider roleProvider : roleProviders) {
					if (roleProvider.acceptType() != userType) {
						continue;
					}
					Role role = roleProvider.getRoleByUserId(userId);
					if (role != null) {
						if (role.getType() == 1) {
							mutableRoles.getRoles().add(role);
						} else
							mutableRoles.getDynamicRoles().add(role);
					}

				}

			}

			postTryUpdateRoles(user, mutableRoles);

			return Response.ok(mutableRoles);
		} catch (Exception e) {
			log.error("hard load rich roles failed, userId={}, cause:{}", userId, Throwables.getStackTraceAsString(e));
		}
		return Response.fail("user.role.load.fail");
	}

	private RoleContent initRoles() {
		RoleContent non = new RoleContent();
		non.setRoles(new ArrayList());
		non.setDynamicRoles(new ArrayList());
		return non;
	}

	private void postTryUpdateRoles(User user, RoleContent roles) {
		Set originRoles = new HashSet();
		if (user.getRoles() != null) {
			originRoles.addAll(user.getRoles());
		}
		Set currentRoles = buildVanillaRole(roles);

		if (!currentRoles.equals(originRoles)) {
			List toUpdateRoles = new ArrayList(currentRoles);
			Response resp = this.adminUserService.updateRoles(user.getId(), toUpdateRoles);
			if (!resp.isSuccess())
				log.warn("try update user roles failed, userId={}, roles={}, error={}",
						new Object[] { user.getId(), toUpdateRoles, resp.getError() });
		}
	}

	private Set<String> buildVanillaRole(RoleContent roles) {
		Set rs = new HashSet();
		for (Role staticRole : roles.getRoles()) {
			rs.add(staticRole.getBase());
		}
		for (Role dynamicRole : roles.getDynamicRoles()) {
			rs.add(dynamicRole.getBase());
		}
		return rs;
	}
}