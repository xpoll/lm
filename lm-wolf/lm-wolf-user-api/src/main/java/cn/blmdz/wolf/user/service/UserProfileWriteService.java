package cn.blmdz.wolf.user.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.user.model.UserProfile;

public interface UserProfileWriteService {
   @Export(
      paramNames = {"profile"}
   )
   Response updateProfile(UserProfile var1);

   @Export(
      paramNames = {"profile"}
   )
   Response createProfile(UserProfile var1);
}
