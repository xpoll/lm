package io.terminus.parana.auth.role;

import io.terminus.parana.auth.role.CustomRoleLoadException;
import java.util.List;

public interface CustomRoleLoader {
   List load(List var1) throws CustomRoleLoadException;
}
