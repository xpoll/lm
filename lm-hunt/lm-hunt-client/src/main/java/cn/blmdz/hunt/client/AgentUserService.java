package cn.blmdz.hunt.client;

import cn.blmdz.home.common.model.BaseUser;

public abstract interface AgentUserService {
	BaseUser getUser(Long userId);
}