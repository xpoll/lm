package cn.blmdz.rabbit.web.core.component;

import java.util.List;

import org.springframework.stereotype.Service;

import com.google.common.collect.Lists;

import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;

/**
 * @author Effet
 */
@Service
public class UserResService {

    @Export(paramNames = {"loginUser"})
    public Response<List<String>> getAllowsOfLoginUser(BaseUser loginUser) {
        List<String> allows = Lists.newArrayList("");
        return Response.ok(allows);
    }
}
