package cn.blmdz.wolf.user.auth;

import java.util.List;

import org.springframework.stereotype.Component;

import com.google.common.collect.Lists;

@Component
public class RoleProviderRegistry {
	private final List<RoleProvider> roleProviders = Lists.newArrayList();

	public void addRoleProvider(RoleProvider roleProvider) {
		if (roleProvider != null)
			this.roleProviders.add(roleProvider);
	}

	public List<RoleProvider> getRoleProviders() {
		return this.roleProviders;
	}
}