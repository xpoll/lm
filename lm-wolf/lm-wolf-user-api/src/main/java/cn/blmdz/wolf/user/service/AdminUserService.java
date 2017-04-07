package cn.blmdz.wolf.user.service;

import java.util.List;
import java.util.Map;

import cn.blmdz.home.common.model.Response;

public interface AdminUserService {
	Response<Boolean> updateTags(Long paramLong, Map<String, String> paramMap);

	Response<Boolean> updateStatus(Long paramLong, Integer paramInteger);

	Response<Boolean> updateRoles(Long paramLong, List<String> paramList);
}