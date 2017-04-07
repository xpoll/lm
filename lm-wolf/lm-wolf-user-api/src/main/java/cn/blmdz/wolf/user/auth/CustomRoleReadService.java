package cn.blmdz.wolf.user.auth;

import java.util.List;

import cn.blmdz.home.common.model.Response;

public interface CustomRoleReadService<R extends CustomRole> {
	Response<List<R>> findByIds(List<Long> paramList);
}