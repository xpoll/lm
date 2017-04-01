package cn.blmdz.hunt.client;

import cn.blmdz.home.common.model.BaseUser;

public interface AgentUserService {
	BaseUser getUser(Long userId);
}