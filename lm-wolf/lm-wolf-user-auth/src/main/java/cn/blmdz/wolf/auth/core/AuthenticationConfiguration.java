package cn.blmdz.wolf.auth.core;

import java.lang.reflect.InvocationTargetException;

import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;

import cn.blmdz.hunt.engine.Setting;
import cn.blmdz.wolf.auth.model.ParanaThreadVars;
import cn.blmdz.wolf.auth.util.ParanaFileLoaderHelper;
import cn.blmdz.wolf.user.auth.UserRoleLoader;

@Configuration
@ComponentScan({ "cn.blmdz.wolf.auth.core", "cn.blmdz.wolf.auth.util" })
@ConditionalOnWebApplication
@EnableAspectJAutoProxy
@EnableConfigurationProperties({ AuthenticationProperties.class })
public class AuthenticationConfiguration {
	@Bean
	@ConditionalOnClass({ Setting.class })
	public AclLoader aclLoader(AuthenticationProperties authenticationProperties, Setting setting,
			ParanaFileLoaderHelper paranaFileLoaderHelper)
			throws ClassNotFoundException, InvocationTargetException, IllegalAccessException {
		authenticationProperties.setDevMode(setting.isDevMode());
		return new AclLoader(authenticationProperties, paranaFileLoaderHelper);
	}

	@Bean
	@ConditionalOnMissingClass({ "cn.blmdz.hunt.engine.Setting" })
	public AclLoader aclLoader(AuthenticationProperties authenticationProperties,
			ParanaFileLoaderHelper paranaFileLoaderHelper) throws ClassNotFoundException {
		ParanaThreadVars.initNoPampasApp(authenticationProperties.getApp());
		return new AclLoader(authenticationProperties, paranaFileLoaderHelper);
	}

	@Bean
	public PermissionHelper permissionHelper(UserRoleLoader userRoleLoader) {
		return new PermissionHelper(userRoleLoader);
	}

	@ConditionalOnBean({ AclLoader.class, PermissionHelper.class })
	@ConditionalOnMissingBean({ Authenticator.class })
	@Bean
	public Authenticator authenticator(AclLoader aclLoader, PermissionHelper permissionHelper,
			AuthenticationProperties authenticationProperties) {
		return new DefaultAuthenticator(aclLoader, permissionHelper, authenticationProperties.getLevel());
	}
}