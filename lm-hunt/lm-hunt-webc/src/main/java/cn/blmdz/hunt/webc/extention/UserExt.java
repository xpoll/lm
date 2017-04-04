package cn.blmdz.hunt.webc.extention;

import cn.blmdz.home.common.model.BaseUser;

public interface UserExt {
	BaseUser getUser(String appKey, Long userId);
}
