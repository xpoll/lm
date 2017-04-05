package io.terminus.parana.user.auth;

import java.util.List;
import java.util.Map;

public interface CustomRole {
   Long getId();

   String getAppKey();

   String getBaseRole();

   boolean isActive();

   List getAllow();

   Map getContext();
}
