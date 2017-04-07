package cn.blmdz.wolf.user.auth;

public interface RoleProvider {
	int acceptType();
	Role getRoleByUserId(Long paramLong);
}