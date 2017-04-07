package cn.blmdz.rabbit.open.common;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.session.AFSessionManager;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.wolf.common.model.ParanaUser;
import cn.blmdz.wolf.user.model.User;
import cn.blmdz.wolf.user.service.UserReadService;
import io.terminus.galaxy.user.util.GalaxyUserMaker;

/**
 * Mail: xiao@terminus.io <br>
 * Date: 2016-03-23 10:42 AM  <br>
 * Author: xiao
 */
@OpenParamHandler(patterns = "*", exclusions = {
        "user.login", "server.time",
        "get.session.id", "get.user.captcher"
})
public class SessionHandle implements ParamPreHandle {

    @Autowired
    private AFSessionManager sessionManager;
    @Autowired
    private UserReadService<User> userReadService;

    /**
     * check params before calling
     *
     * @param params params from http request
     * @throws OPClientException
     */
    @Override
    public void handle(Map<String, Object> params) throws OPClientException {
        if (params.get("sid") == null || isEmpty((String)params.get("sid"))) {
            throw new OPClientException("session.id.miss");
        }
        String sessionId = params.get("sid").toString();
        Map<String, Object> snapshot = sessionManager.findSessionById(Sessions.TOKEN_PREFIX, sessionId);
        if (snapshot == null || snapshot.size() == 0 || snapshot.get(Sessions.USER_ID) == null) {
            throw new OPClientException(400, "session.id.expired");
        }
        // refresh
        sessionManager.refreshExpireTime(Sessions.TOKEN_PREFIX, sessionId, Sessions.LONG_INACTIVE_INTERVAL);
        Long uid = Long.parseLong(snapshot.get(Sessions.USER_ID).toString());
        Response<User> res = userReadService.findById(uid);
        if (!res.isSuccess()) {
            throw new OPClientException(400, res.getError());
        }

        ParanaUser paranaUser = GalaxyUserMaker.from(res.getResult());
        UserUtil.putCurrentUser(paranaUser);
    }
}
