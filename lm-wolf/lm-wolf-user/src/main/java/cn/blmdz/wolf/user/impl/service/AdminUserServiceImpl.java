package cn.blmdz.wolf.user.impl.service;

import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.user.impl.dao.UserDao;
import cn.blmdz.wolf.user.service.AdminUserService;

@Service
public class AdminUserServiceImpl implements AdminUserService {
	private static final Logger log = LoggerFactory.getLogger(AdminUserServiceImpl.class);
	private final UserDao userDao;

	@Autowired
	public AdminUserServiceImpl(UserDao userDao) {
		this.userDao = userDao;
	}

	public Response<Boolean> updateTags(Long userId, Map<String, String> tags) {
		try {
			this.userDao.updateTags(userId, tags);
			return Response.ok(Boolean.TRUE);
		} catch (Exception e) {
			log.error("failed to update tags to {} for user(id={}), cause:{}",
					new Object[] { tags, userId, Throwables.getStackTraceAsString(e) });
		}
		return Response.fail("user.tags.update.fail");
	}

	public Response<Boolean> updateStatus(Long userId, Integer status) {
		try {
			this.userDao.updateStatus(userId, status);
			return Response.ok(Boolean.TRUE);
		} catch (Exception e) {
			log.error("failed to update status to {} for user(id={}), cause:{}",
					new Object[] { status, userId, Throwables.getStackTraceAsString(e) });
		}
		return Response.fail("user.status.update.fail");
	}

	public Response<Boolean> updateRoles(Long userId, List<String> roles) {
		try {
			this.userDao.updateRoles(userId, roles);
			return Response.ok(Boolean.TRUE);
		} catch (Exception e) {
			log.error("update user roles failed, userId={}, roles={}, cause:{}",
					new Object[] { userId, roles, Throwables.getStackTraceAsString(e) });
		}
		return Response.fail("user.roles.update.fail");
	}
}