package io.terminus.parana.user.service;

import io.terminus.common.model.Response;
import io.terminus.pampas.client.Export;
import io.terminus.parana.user.model.UserProfile;

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
