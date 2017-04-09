package cn.blmdz.wolf.auth.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

import lombok.Data;

import java.io.Serializable;
import java.util.List;

@Data
@JsonInclude(Include.NON_NULL)
public class PermissionData implements Serializable {
   private static final long serialVersionUID = 1L;
   private List<Request> requests;
   private List<Request> allRequests;
   private List<String> resources;
}
