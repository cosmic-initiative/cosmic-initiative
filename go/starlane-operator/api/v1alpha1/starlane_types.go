/*
Copyright 2021.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

package v1alpha1

import (
	corev1 "k8s.io/api/core/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
)

// EDIT THIS FILE!  THIS IS SCAFFOLDING FOR YOU TO OWN!
// NOTE: json tags are required.  Any new fields you add must have json tags for the fields to be serialized.

// StarlaneSpec defines the desired state of Starlane
type StarlaneSpec struct {
	// INSERT ADDITIONAL SPEC FIELDS - desired state of cluster
	// Important: Run "make" to regenerate code after modifying this file

	DisableStarlaneDeployment bool               `json:"disable-starlane-deployment,omitempty,default=false"`
	Image                     string             `json:"image,omitempty"`
	PostgresServiceType       corev1.ServiceType `json:"postgres-service-type,omitempty,default=ClusterIP" protobuf:"bytes,4,opt,name=postgres-service-type,casttype=ServiceType,default=ClusterIP"`
	KeycloakServiceType       corev1.ServiceType `json:"keycloak-service-type,omitempty,default=ClusterIP" protobuf:"bytes,4,opt,name=keycloak-service-type,casttype=ServiceType,default=ClusterIP"`
	WebServiceType            corev1.ServiceType `json:"web-service-type,omitempty" protobuf:"bytes,4,opt,name=web-service-type,casttype=ServiceType"`
	GatewayServiceType        corev1.ServiceType `json:"gateway-service-type,omitempty" protobuf:"bytes,4,opt,name=gateway-service-type,casttype=ServiceType"`
	StorageClass              string             `json:"storage-class"`
}

// StarlaneStatus defines the observed state of Starlane
type StarlaneStatus struct {
	// INSERT ADDITIONAL STATUS FIELD - define observed state of cluster
	// Important: Run "make" to regenerate code after modifying this file
}

//+kubebuilder:object:root=true
//+kubebuilder:subresource:status

// Starlane is the Schema for the starlanes API
//+kubebuilder:subresource:status
type Starlane struct {
	metav1.TypeMeta   `json:",inline"`
	metav1.ObjectMeta `json:"metadata,omitempty"`

	Spec   StarlaneSpec   `json:"spec,omitempty"`
	Status StarlaneStatus `json:"status,omitempty"`
}

//+kubebuilder:object:root=true

// StarlaneList contains a list of Starlane
type StarlaneList struct {
	metav1.TypeMeta `json:",inline"`
	metav1.ListMeta `json:"metadata,omitempty"`
	Items           []Starlane `json:"items"`
}

func init() {
	SchemeBuilder.Register(&Starlane{}, &StarlaneList{})
}
